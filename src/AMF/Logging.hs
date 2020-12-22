{-|
Module      : AMF.Logging
Description :
Copyright   : (c) 2020 Adam Flott
License     : NONE
Maintainer  : adam@adamflott.com
Portability :

-}
module AMF.Logging
    (
      -- * Construction
      newConfig
    , newLoggingCtx

    -- * Outputs
    , newEmptyOutputs
    , addConsoleLogger

    -- | Operations
    , startLogger
    , stopLogger
   -- , logRotate


    -- * Log Events
    , logAMFEvent
    , logEvent

    -- * Misc.
--    , getConfig
    , getOutputs
    , module X
    ) where

-- prelude
import           Relude

-- base

-- Hackage
import           BroadcastChan
import           Chronos
import           Control.Lens
import           Network.HostName               ( getHostName )
import           System.Posix.Process
import           System.Posix.User              ( getLoginName )
import           UnliftIO.Concurrent            ( ThreadId
                                                , myThreadId
                                                )
import           Control.Concurrent.Async.Lifted
                                                ( async
                                                , cancel
                                                )
import           Control.Monad.Catch            ( MonadMask )


-- local
import           AMF.Logging.Types
import           AMF.Logging.Types.Level
import           AMF.Logging.Types.Outputs
import           AMF.Logging.Types.OutputsInterface
import           AMF.Logging.Types.Console
import           AMF.Logging.Types.File
import           AMF.Logging.Types.Format      as X


import           AMF.Types.FileSystem
import           AMF.Events


getConfig :: MonadIO m => DynamicLoggerConfig -> m LoggerConfig
getConfig (DynamicLoggerConfig t_cfg) = do
    cfg <- readTVarIO t_cfg
    pure cfg


getOutputs :: MonadIO m => LoggerCtx ev -> m LogOutputs
getOutputs ctx = do
    let dcfg = ctx ^. loggingCtxCfg

    cfg <- getConfig dcfg

    let t_outs = cfg ^. loggingCtxOutputs

    outs <- readTVarIO t_outs
    pure outs

getConsoleOutputs :: MonadIO m => LoggerCtx ev -> m [LogOutputConsole]
getConsoleOutputs ctx = do
    outs <- getOutputs ctx
    pure (outs ^. logOutputsConsoles)

getFileOutputs :: MonadIO m => LoggerCtx ev -> m [LogOutputFile]
getFileOutputs ctx = do
    outs <- getOutputs ctx
    pure (outs ^. logOutputsFiles)


newConfig :: MonadIO m => LogOutputs -> m DynamicLoggerConfig
newConfig outs = do
    t_outs <- newTVarIO outs
    cfg    <- atomically (newTVar (LoggerConfig t_outs))
    pure (DynamicLoggerConfig cfg)

newEmptyOutputs :: LogOutputs
newEmptyOutputs = LogOutputs [] []


-- | Create a new logging context when given a daemon config and a relative file name
--
-- The full path to the file is built from the daemon config and relative file name.
newLoggingCtx :: MonadIO m => DynamicLoggerConfig -> m (LoggerCtx e)
newLoggingCtx dcfg = do
    ch_int_in    <- newBroadcastChan
    ch_int_out   <- newBChanListener ch_int_in

    outs_console <- newTVarIO []
    outs_file    <- newTVarIO []

    pid          <- liftIO getProcessID
    hn           <- liftIO getHostName
    ln           <- liftIO getLoginName

    pure (LoggerCtx (HostName (toText hn)) (UserName (toText ln)) (ProcessId pid) dcfg ch_int_in ch_int_out outs_console outs_file)


-- | Spawn a thread and start reading from the logging channel.
startLogger :: (MonadIO m, MonadMask m, MonadFileSystemRead m, MonadFileSystemWrite m, Show ev, Eventable ev) => LoggerCtx ev -> m LoggerHandle
startLogger ctx = do
    console_outs <- getConsoleOutputs ctx
    file_outs    <- getFileOutputs ctx

    x            <- mapM openOutput console_outs
    let (errs_console, out_handles_console) = partitionEithers x

    y <- mapM openOutput file_outs
    let (errs_file, out_handles_file) = partitionEithers y

    logAMFEvent ctx LogLevelTerse AMFEvLogEventOpen

    atomically (modifyTVar' (ctx ^. loggingCtxOutputHandlesConsole) (++ out_handles_console))
    atomically (modifyTVar' (ctx ^. loggingCtxOutputHandlesFile) (++ out_handles_file))

    liftIO $ async $ loggerWorker ctx

stopLogger :: (MonadIO m, MonadMask m, MonadFileSystemRead m, MonadFileSystemWrite m, Eventable ev, Show ev) => LoggerCtx ev -> LoggerHandle -> m ()
stopLogger ctx handle = do
    logAMFEvent ctx LogLevelTerse AMFEvLogEventClose

    void (closeBChan (ctx ^. loggingCtxIntInEv))

    -- First stop the worker thread, prevents any further writes to channel
    liftIO (cancel handle)

    -- drain
    drainChan ctx

    handles_file <- readTVarIO (ctx ^. loggingCtxOutputHandlesFile)

--    closeOutputs handles_console
    closeOutputs handles_file

addConsoleLogger :: MonadIO m => LoggerCtx ev -> OutputHandle LogOutputConsole -> m ()
addConsoleLogger log_ctx out = do
    atomically (modifyTVar' (log_ctx ^. loggingCtxOutputHandlesConsole) (++ [out]))

loggerWorker :: (MonadIO m, MonadMask m, MonadFileSystemRead m, MonadFileSystemWrite m, Show ev, Eventable ev) => LoggerCtx ev -> m ()
loggerWorker ctx = do
    let hn  = ctx ^. loggingCtxHostName
        ln  = ctx ^. loggingCtxUserName
        pid = ctx ^. loggingCtxProcessId

    handles_console <- readTVarIO (ctx ^. loggingCtxOutputHandlesConsole)
    handles_file    <- readTVarIO (ctx ^. loggingCtxOutputHandlesFile)

    maybe_ev        <- readBChan (ctx ^. loggingCtxIntOutEv)
    case maybe_ev of
        Nothing -> pass
        Just (LogEventWithDetails ts tid lvl ev) -> do
            writeOutputs handles_console hn ln ts (pid, tid) lvl ev
            writeOutputs handles_file    hn ln ts (pid, tid) lvl ev
            loggerWorker ctx


writeOutputs
    :: (Foldable t, Output m a1, Eventable a2)
    => t (OutputHandle a1)
    -> HostName
    -> UserName
    -> Time
    -> (ProcessId, ThreadId)
    -> LogLevel
    -> a2
    -> m ()
writeOutputs handles hn ln ts (pid, tid) lvl ev = do
    mapM_
        (\h -> do
            fmt <- outputFormat h
            whenJust (toFmt fmt hn ln ts (pid, tid) lvl ev) $ \line -> do
                writeOutput h line
        )
        handles

closeOutputs :: (Foldable t, Output m a1) => t (OutputHandle a1) -> m ()
closeOutputs handles = do
    mapM_
        (\h -> do
            closeOutput h
        )
        handles


logAMFEvent :: (MonadIO m) => LoggerCtx ev -> LogLevel -> AMFEvent -> m ()
logAMFEvent ctx lvl ev = do
    let ch = ctx ^. loggingCtxIntInEv
    ts  <- liftIO now
    tid <- myThreadId
    void $ writeBChan ch (LogEventWithDetails ts tid lvl (LogCmdAddAMFEv ev))

logEvent :: (MonadIO m) => LoggerCtx ev -> LogLevel -> ev -> m ()
logEvent ctx lvl ev = do
    let ch = ctx ^. loggingCtxIntInEv
    ts  <- liftIO now
    tid <- myThreadId
    void $ writeBChan ch (LogEventWithDetails ts tid lvl (LogCmdAddEv ev))


drainChan :: (MonadIO m, MonadMask m, MonadFileSystemRead m, MonadFileSystemWrite m, Eventable ev, Show ev) => LoggerCtx ev -> m ()
drainChan ctx = do
    let ch  = ctx ^. loggingCtxIntOutEv
        hn  = ctx ^. loggingCtxHostName
        ln  = ctx ^. loggingCtxUserName
        pid = ctx ^. loggingCtxProcessId

    maybe_ev <- readBChan ch
    case maybe_ev of
        Nothing -> pass
        Just (LogEventWithDetails ts tid lvl ev) -> do
            handles_console <- readTVarIO (ctx ^. loggingCtxOutputHandlesConsole)
            handles_file    <- readTVarIO (ctx ^. loggingCtxOutputHandlesFile)
            writeOutputs handles_console hn ln ts (pid, tid) lvl ev
            writeOutputs handles_file    hn ln ts (pid, tid) lvl ev
            drainChan ctx


{-
-- | Rotate log from temporary file name to final name.
logRotate :: MonadIO m => LoggerCtx e -> m ()
logRotate ctx = do
    let ch = _loggingCtxInEv ctx
    void $ writeBChan ch LogCmdRotate

processLogRotate :: MonadIO m => LoggerCtx e -> m (Either Exception.IOException ())
processLogRotate ctx = do
    cfg <- readTVarIO udc
    let log_dir = cfg ^. sgdCfgLogging . sgdCfgLoggingDir
        log_fp  = log_dir </> fp

    -- create a temporary file with a unique name to avoid opening a file that
    -- may not have been fully moved yet to avoid using a file that akarotate
    -- will then manage
    maybe_new_fh <- liftIO $ Exception.tryIO (openTempFile (toFilePath log_dir) (toFilePath fp <> "XXXX"))
    case maybe_new_fh of
        Left err -> do
            putStrLn ("logRotate error: " <> show err)
            pure (Left err)
        Right (tmp_fp, new_fh) -> do
            new_fd <- liftIO $ handleToFd new_fh
            logEvent ctx (EventLogOpen True (show new_fd))
            configureHandle new_fh

            old_fh        <- atomically (swapTVar th new_fh)
            old_fd        <- liftIO $ handleToFd old_fh
            rename_result <- liftIO $ Exception.tryIO (renameFile tmp_fp (toFilePath log_fp))
            case rename_result of
                Left err -> do
                    putStrLn ("logRotate rename() error(): " <> show err)
                    pure (Left err)
                Right _ -> do
                    flush_result <- liftIO $ Exception.tryIO $ do
                        hFlush old_fh
                        logEvent ctx (EventLogFlush False (show old_fd))

                        hClose old_fh
                        logEvent ctx (EventLogClose False (show old_fd))

                    case flush_result of
                        Left err -> do
                            putStrLn ("logRotate flush() and close error(): " <> show err)
                            pure (Left err)
                        Right _ -> pure (Right ())
-}
