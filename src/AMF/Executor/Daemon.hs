module AMF.Executor.Daemon where

import           Relude

-- base

-- Hackage
import           Chronos
import           Path
import           Main.Utf8
import           Control.Concurrent.Async
import           Control.Lens
import           System.FSNotify
import qualified Data.Map.Strict               as Map

-- local
import           AMF.API
import           AMF.API.Log
import           AMF.Events
import           AMF.Executor.Common           as Common
import           AMF.Logging
import           AMF.Logging.Outputs.Console
import           AMF.Logging.Types.Console
import           AMF.Logging.Types.Level
import           AMF.Types.Environment
import           AMF.Types.Executor
import           AMF.Types.FileSystem
import           AMF.Types.RunCtx


data Traditional = Traditional
    { _appName         :: Text
    , _fsNotifyManager :: WatchManager
    , _fsNotifyListner :: Maybe StopListening
    }
    deriving stock Generic


instance Executor Traditional where
    fsDirRoot _ = Just [absdir|/tmp|]

    fsDirMetadata (Traditional app_name _ _) = (parseRelDir (toString ("etc/" <> app_name)))
    fsDirLogs _ = Just [reldir|var/logs|]

    fsFileAppInfo _ = Nothing

    fsDirCache _ = Just [reldir|var/cache|]

    getExec a = pure a

    initExec _run_ctx = do
        pn <- getProgName

        m  <- liftIO startManager

        pure (Right (Traditional pn m Nothing))

    setupExec = setup

    finishExec run_ctx ctx@(Traditional _ m _) = do
        liftIO (stopManager m)

        case fsDirJoin (fsDirRoot ctx) [fsDirMetadata ctx] of
            Nothing -> pass
            Just d  -> do
                AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvFSNotifyUnWatch (toText (toFilePath d)))

        pure (Right ctx)

configFilename :: (ToString a, Semigroup a, IsString a) => RunCtx ev cfg -> a -> Path Rel File
configFilename run_ctx pn = case (run_ctx ^. runCtxConfigParser) of
    ConfigParserYAML _ -> case parseRelFile (toString (pn <> ".yaml")) of
        Nothing -> [relfile|daemon.yaml|]
        Just v  -> v

setup :: (IsString a, MonadIO m, MonadEventLogger m, MonadFileSystemRead m) => RunCtx ev cfg -> Traditional -> m (Either a Traditional)
setup run_ctx ctx@(Traditional pn m _) = do
    let maybe_dir = fsDirJoin (fsDirRoot ctx) [fsDirMetadata ctx]
    let fn        = configFilename run_ctx pn
    let f         = fsFileJoin maybe_dir fn
    parse maybe_dir f

  where
    parse (Just d) (Just fn) = do
        r <- readAndParse run_ctx fn
        store d (toText (toFilePath fn)) r
    parse Nothing _ = do
        pure (Right (Traditional pn m Nothing))

    store _ _  (Left  err) = pure (Left (show err))
    store d fn (Right cfg) = do
        storeX run_ctx fn cfg
        l <- liftIO (watchDir m (toFilePath d) (const True) (configChangeHandler run_ctx))
        AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvFSNotifyWatch (toText (toFilePath d)))

        pure (Right (Traditional pn m (Just l)))


storeX :: (MonadIO m, MonadEventLogger m) => RunCtx ev cfg -> Text -> cfg -> m ()
storeX run_ctx fn cfg = do
    liftIO $ atomically $ modifyTVar' (run_ctx ^. runCtxConfig) $ \cfg_map -> do
        case Map.lookup fn cfg_map of
            Nothing -> Map.insert (fn) cfg cfg_map
            Just _  -> Map.adjust (\_ -> cfg) (fn) cfg_map
    AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvConfigStore)

readAndParse :: (Monad m, MonadFileSystemRead m, MonadEventLogger m) => RunCtx ev cfg -> Path b1 File -> m (Either ConfigParseResult cfg)
readAndParse run_ctx fp = do
    maybe_read <- AMF.Types.FileSystem.readFile fp
    AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvConfigRead)
    case maybe_read of
        Left  err      -> pure (Left (ConfigParseResultIO (show err)))
        Right contents -> do
            AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvConfigParse)
            case (run_ctx ^. runCtxConfigParser) of
                ConfigParserYAML parser -> pure (parser contents)


configChangeHandler :: (MonadIO m, MonadFileSystemRead m, MonadEventLogger m) => RunCtx ev cfg -> Event -> m ()
configChangeHandler run_ctx fs_ev = do
    AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvConfigFSEvent fs_ev)
    whenJust (parseAbsFile (eventPath fs_ev)) $ \fp -> do
        maybe_cfg <- readAndParse run_ctx fp
        case maybe_cfg of
            Left err -> do
                print err
                pass
            Right cfg -> do
                storeX run_ctx (toText (toFilePath fp)) cfg
    pass

{-
runDaemon
    :: (Show ev, Eventable ev)
    => (Traditional -> RunCtx ev -> t1 -> IO (Either ExitCode t2))
    -> (RunCtx ev -> t2 -> IO t3)
    -> (RunCtx ev -> t3 -> IO ())
    -> t1
    -> IO ()
    -}
runDaemon app_name cfg_parser app_setup app_main app_finish opts = withUtf8 $ do
    run_ctx <- Common.init app_name cfg_parser
    let log_ctx = run_ctx ^. runCtxLogger

    log_h               <- startLogger log_ctx
    maybe_logger_stdout <- newConsoleOutput LogLevelAll LogFormatLine LogOutputStdOut
    case maybe_logger_stdout of
        Left  _             -> pass
        Right logger_stdout -> addLogger log_ctx logger_stdout

    maybe_t <- initExec run_ctx
    case maybe_t of
        Left err -> do
            print err
            pass
        Right (t :: Traditional) -> do

            setup_exec_result <- setupExec run_ctx t
            case setup_exec_result of
                Left err -> do
                    print err
                    _ <- finishExec run_ctx t
                    pass
                Right t' -> do
                    (uptime, result) <- stopwatch $ do

                        logAllSysInfo run_ctx t'

                        AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvStart amfVersion)
                        AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvPhase Setup)

                        setup_result <- app_setup t' run_ctx opts
                        case setup_result of
                            Left ec -> do
                                Relude.exitWith ec
                            Right setup_val -> do
                                AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvPhase Main)
                                app_handle <- async (app_main t' run_ctx setup_val)

                                main_val   <- wait app_handle

                                AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvPhase Finish)
                                app_finish run_ctx main_val

                    AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvStop amfVersion uptime)

                    _ <- finishExec run_ctx t'

                    stopLogger log_ctx log_h

                    pure result
