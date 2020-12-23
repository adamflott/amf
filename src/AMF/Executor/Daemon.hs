module AMF.Executor.Daemon where

import           Relude

-- base

-- Hackage
import           Control.Lens
import           Path
import           System.FSNotify
import qualified Data.Map.Strict               as Map

-- local
import           AMF.API
import           AMF.Events
import           AMF.Executor.Common           as Common
import           AMF.Logging.Types.Level
import           AMF.Types.AppSpec
import           AMF.Types.Config
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

configFilename :: ToString a => RunCtx ev opts cfg -> a -> Path Rel File
configFilename run_ctx pn = case (run_ctx ^. runCtxConfigParser) of
    ConfigParser ext _ -> case parseRelFile (toString (toString pn <> "." <> toString ext)) of
        Nothing -> [relfile|daemon.yaml|]
        Just v  -> v

setup :: (IsString a, MonadIO m, MonadEventLogger m, MonadFileSystemRead m) => RunCtx ev opts cfg -> Traditional -> m (Either a Traditional)
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

    store _ _  (Left  err) = pure (Left ((show err)))
    store d fn (Right cfg) = do
        storeX run_ctx fn cfg
        l <- liftIO (watchDir m (toFilePath d) (const True) (configChangeHandler run_ctx))
        AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvFSNotifyWatch (toText (toFilePath d)))

        pure (Right (Traditional pn m (Just l)))


storeX :: (MonadIO m, MonadEventLogger m) => RunCtx ev opts cfg -> Text -> cfg -> m ()
storeX run_ctx fn cfg = do
    liftIO $ atomically $ modifyTVar' (run_ctx ^. runCtxConfig) $ \cfg_map -> do
        case Map.lookup fn cfg_map of
            Nothing -> Map.insert (fn) cfg cfg_map
            Just _  -> Map.adjust (\_ -> cfg) (fn) cfg_map
    AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvConfigStore)

readAndParse :: (Monad m, MonadFileSystemRead m, MonadEventLogger m) => RunCtx ev opts cfg -> Path b1 File -> m (Either (ConfigParseErr) cfg)
readAndParse run_ctx fp = do
    maybe_read <- AMF.Types.FileSystem.readFile fp
    AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvConfigRead)
    case maybe_read of
        Left  err      -> pure (Left (ConfigParseErrIO (show err)))
        Right contents -> do
            AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvConfigParse)
            case (run_ctx ^. runCtxConfigParser) of
                ConfigParser _ parser -> pure (parser contents)


configChangeHandler :: (MonadIO m, MonadFileSystemRead m, MonadEventLogger m) => RunCtx ev opts cfg -> Event -> m ()
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


runAppSpecAsDaemon :: (Eventable ev, Show ev, Show cfg) => AppSpec IO Traditional ev opts cfg a -> IO ()
runAppSpecAsDaemon = runAppSpec
