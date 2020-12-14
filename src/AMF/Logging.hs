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

    -- | Operations
    , startLogger
    , stopLogger
   -- , logRotate


    -- * Log Events
    , logEvent


    -- * Misc.
    , getConfig
    , getOutputs

--    , withLogger
    , module X
    )
where

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
import           UnliftIO.Concurrent            ( ThreadId, myThreadId )
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
import AMF.Logging.Types.Format as X


import           AMF.Types.FileSystem


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

    ch_ext_in    <- newBroadcastChan
    ch_ext_out   <- newBChanListener ch_ext_in

    outs_console <- newTVarIO []
    outs_file    <- newTVarIO []

    pid          <- liftIO getProcessID
    hn           <- liftIO getHostName
    ln           <- liftIO getLoginName

    pure (LoggerCtx (HostName (toText hn)) (UserName (toText ln)) (ProcessId pid) dcfg ch_int_in ch_int_out ch_ext_in ch_ext_out outs_console outs_file)


-- | Spawn a thread and start reading from the logging channel.
startLogger :: (MonadIO m, MonadMask m, MonadFileSystemRead m, MonadFileSystemWrite m, Show ev, Eventable ev) => LoggerCtx ev -> m LoggerHandle
startLogger ctx = do
    console_outs <- getConsoleOutputs ctx
    file_outs    <- getFileOutputs ctx

    x            <- mapM openOutput console_outs
    let (errs_console, out_handles_console) = partitionEithers x

    y <- mapM openOutput file_outs
    let (errs_file, out_handles_file) = partitionEithers y

    logInternalEvent ctx LogEventOpen

    atomically (modifyTVar' (ctx ^. loggingCtxOutputHandlesConsole) (++ out_handles_console))
    atomically (modifyTVar' (ctx ^. loggingCtxOutputHandlesFile) (++ out_handles_file))

    liftIO $ async $ loggerWorker ctx

stopLogger :: (MonadIO m, MonadMask m, MonadFileSystemRead m, MonadFileSystemWrite m, Eventable ev, Show ev) => LoggerCtx ev -> LoggerHandle -> m ()
stopLogger ctx handle = do
    logInternalEvent ctx LogEventClose

    void (closeBChan (ctx ^. loggingCtxIntInEv))
    void (closeBChan (ctx ^. loggingCtxExtInEv))

    -- First stop the worker thread, prevents any further writes to channel
    liftIO (cancel handle)

    -- drain
    drainChan ctx

    handles_file    <- readTVarIO (ctx ^. loggingCtxOutputHandlesFile)

--    closeOutputs handles_console
    closeOutputs handles_file

loggerWorker :: (MonadIO m, MonadMask m, MonadFileSystemRead m, MonadFileSystemWrite m, Show ev, Eventable ev) => LoggerCtx ev -> m ()
loggerWorker ctx = do
   -- maybe_cmd <- readBChan (ctx ^. loggingCtxIntOutEv)
   -- handleInternalCh ctx maybe_cmd

    let hn = ctx ^. loggingCtxHostName
        ln = ctx ^. loggingCtxUserName
        pid = ctx ^. loggingCtxProcessId

    handles_console <- readTVarIO (ctx ^. loggingCtxOutputHandlesConsole)
    handles_file    <- readTVarIO (ctx ^. loggingCtxOutputHandlesFile)

    maybe_ev        <- readBChan (ctx ^. loggingCtxExtOutEv)
    case maybe_ev of
        Nothing                              -> pass
        Just (LogEventWithDetails ts tid lvl ev) -> do
            writeOutputs handles_console hn ln ts (pid, tid) lvl ev
            writeOutputs handles_file    hn ln ts (pid, tid) lvl ev
            loggerWorker ctx

writeOutputs :: (Foldable t, Output m a1, Eventable a2) => t (OutputHandle a1) -> HostName -> UserName -> Time -> (ProcessId, ThreadId) -> LogLevel -> a2 -> m ()
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

{-
handleInternalCh
    :: (MonadIO m, MonadMask m, MonadFileSystemRead m, MonadFileSystemWrite m, Show ev, Eventable ev)
    => LoggerCtx ev
    -> Maybe (LogEventWithDetails LogCmd)
    -> m ()
handleInternalCh ctx maybe_cmd = do
    handles_console <- readTVarIO (ctx ^. loggingCtxOutputHandlesConsole)
    handles_file    <- readTVarIO (ctx ^. loggingCtxOutputHandlesFile)

    case maybe_cmd of
        Nothing                               -> do
          putTextLn "nothing"
          pass
        Just (LogEventWithDetails ts tid cmd) -> case cmd of
            LogCmdRotate -> do
                loggerWorker ctx
            LogCmdAdd ev -> do
                writeOutputs handles_console ts tid ev
                writeOutputs handles_file    ts tid ev
                loggerWorker ctx
-}

logInternalEvent :: (MonadIO m) => LoggerCtx ev -> LogEventInternal -> m ()
logInternalEvent ctx ev = do
    let ch = ctx ^. loggingCtxIntInEv
    ts  <- liftIO now
    tid <- myThreadId
    void $ writeBChan ch (LogEventWithDetails ts tid LogLevelAll (LogCmdAdd ev))


logEvent :: (MonadIO m, Show ev, Eventable ev) => LoggerCtx ev -> LogLevel -> ev -> m ()
logEvent ctx lvl ev = do
    let ch = ctx ^. loggingCtxExtInEv
    ts  <- liftIO now
    tid <- myThreadId
    void $ writeBChan ch (LogEventWithDetails ts tid lvl ev)


drainChan :: (MonadIO m, MonadMask m, MonadFileSystemRead m, MonadFileSystemWrite m, Eventable ev, Show ev) => LoggerCtx ev -> m ()
drainChan ctx = do
    let ch = ctx ^. loggingCtxExtOutEv
        hn = ctx ^. loggingCtxHostName
        ln = ctx ^. loggingCtxUserName
        pid = ctx ^. loggingCtxProcessId

    maybe_ev <- readBChan ch
    case maybe_ev of
        Nothing                              -> pass
        Just (LogEventWithDetails ts tid lvl ev) -> do
            handles_console <- readTVarIO (ctx ^. loggingCtxOutputHandlesConsole)
            handles_file    <- readTVarIO (ctx ^. loggingCtxOutputHandlesFile)
            writeOutputs handles_console hn ln ts (pid, tid) lvl ev
            writeOutputs handles_file   hn ln  ts (pid, tid) lvl ev
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


-- base does not expose the underlying thread id, so we have to strip the unnecessary prefix
truncateThreadId :: ThreadId -> LText
truncateThreadId tid = fromMaybe "" (LText.stripPrefix "ThreadId " (show tid))

fmtEv :: ProcessID -> LogLevel -> LogEventWithDetails a -> Text
fmtEv pid lvl (LogEventWithDetails ts tid ev) = do
    let ts_fmt = builder_YmdHMS SubsecondPrecisionAuto w3c (timeToDatetime ts)
    toText $ toLazyText ts_fmt <> " " <> show pid <> "/" <> truncateThreadId tid <> " " <> toLText (eventToString lvl ev)


-- | Wrapper for use with small utilities
withLogger :: Path Rel File -> (LoggerCtx e -> IO ()) -> IO ()
withLogger log_filename f = do
    cfg <- newDaemonConfig defaultDaemonConfig
    ctx <- newLoggingCtx cfg log_filename
    h   <- startLogger ctx
    f ctx
    stopLogger ctx h

loggingConfigLevelToType :: Text -> LogLevel
loggingConfigLevelToType = \case
    "terse" -> LogLevelTerse
    _       -> LogLevelAll
-}
