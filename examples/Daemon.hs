module Main where

-- prelude
import           Relude

-- base
import           Control.Concurrent             ( threadDelay )
import           System.Exit

-- Hackage
import           Codec.Serialise               as CBOR
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad.Catch            ( MonadMask )
import           Options.Applicative
import qualified Data.Aeson                    as Aeson
import qualified System.Posix                  as Posix
import qualified Data.YAML                     as YAML
import           Path

-- local
import           AMF.API
import           AMF.Events
import           AMF.Executor.Daemon
import           AMF.Logging.Types
import           AMF.Logging.Types.Format
import           AMF.Logging.Types.Level
import           AMF.Types.Common
import           AMF.Types.Executor
import           AMF.Types.RunCtx
import           AMF.Types.AppSpec
import           AMF.Types.Config


data DaemonType
    = DaemonTypeTraditional
    | DaemonTypeKubernetes
    deriving stock (Show)

data Options = Options
    { dt         :: DaemonType
    , configFile :: Text
    }
    deriving stock Show



optSpec :: Parser Options
optSpec = Options <$> (dtTrad <|> dtK8s) <*> configFileParser
  where
    dtTrad           = flag' DaemonTypeTraditional (long "traditional" <> short 't' <> help "")
    dtK8s            = flag' DaemonTypeKubernetes (long "kubernetes" <> short 'k' <> help "")
    configFileParser = strOption (long "config" <> short 'c' <> metavar "FILE" <> value "config.yaml" <> showDefault <> help "Config file path")


--------------------------------------------------------------------------------

sigHandler :: RunCtx EventX Config -> Posix.Signal -> IO ()
sigHandler _run_ctx _sig = do
    pass

--------------------------------------------------------------------------------

data EventX = EventConfig Config
    deriving stock (Generic, Show)
    deriving anyclass (Serialise, Aeson.ToJSON)



instance Eventable EventX where
    toFmt fmt hn ln ts (pid, tid) lvl ev = case fmt of
        LogFormatLine -> Just (defaultLinePrefixFormatter hn ln ts (pid, tid) lvl <+> daemonEvLineFmt ev)
        LogFormatJSON -> Just (Aeson.encode ev <> "\n")
        LogFormatCBOR -> Just (CBOR.serialise ev)
        LogFormatCSV  -> Nothing

daemonEvLineFmt :: EventX -> LByteString
daemonEvLineFmt ev = evFmt ev <> "\n"
  where
    evFmt = \case
        EventConfig cfg -> "cfg:" <> show cfg

--------------------------------------------------------------------------------

data Config = Config
    { i :: Int
    , s :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (Serialise, Aeson.ToJSON)

instance YAML.FromYAML Config where
    parseYAML = YAML.withMap "Example Daemon Config" $ \m -> Config <$> m YAML..: "i" <*> m YAML..: "s"


cfgParser :: ConfigParser Config
cfgParser = yamlParser

--------------------------------------------------------------------------------

type AppConstraints m
    = ( MonadIO m
      , MonadMask m
      , MonadFail m
      , MonadTime m
      , MonadEventLogger m
      , MonadLoggerConsoleAdd m
      , MonadUnixSignals m
      , MonadUnixSignalsRaise m
      , MonadEventQueueRead m
      , MonadEventQueueListen m
      , MonadConfigGet m
      , MonadConfigChangeBlockingReact m
      )

getAppConfigFilepath :: Executor a => a -> RunCtx ev cfg -> Text
getAppConfigFilepath exec run_ctx = do
    let d        = fsDirJoin (fsDirRoot exec) [fsDirMetadata exec]
        fn       = configFilename run_ctx (run_ctx ^. runCtxAppName)
        maybe_fp = fsFileJoin d fn

    maybe "?" (toText . toFilePath) maybe_fp

myAppSetup :: (AppConstraints m, Executor e) => e -> RunCtx EventX Config -> Options -> m (Either ExitCode Options)
myAppSetup exec run_ctx opts = do

    setConfigDefault run_ctx Nothing
    setConfigBlockingReadAndParseFor run_ctx (configFile opts)

    let app_cfg_fp = getAppConfigFilepath exec run_ctx
    maybe_cfg <- getConfig run_ctx app_cfg_fp

    whenJust maybe_cfg $ \cfg -> do
        logEvent run_ctx LogLevelTerse (EventConfig cfg)

    addSignalHandler run_ctx [Posix.sigHUP, Posix.sigTERM, Posix.sigINT] sigHandler
    raiseSignal run_ctx Posix.sigHUP

    pure (Right opts)


heartbeat :: (MonadIO m, MonadEventLogger m, MonadConfigGet m, Executor e) => e -> RunCtx EventX Config -> m ()
heartbeat exec run_ctx = do
    liftIO $ threadDelay (10 * 1000000)

    let app_cfg_fp = getAppConfigFilepath exec run_ctx
    maybe_cfg <- getConfig run_ctx app_cfg_fp

    whenJust maybe_cfg $ \cfg -> do
        logEvent run_ctx LogLevelTerse (EventConfig cfg)

    heartbeat exec run_ctx

myAppMain :: (AppConstraints m, Executor e) => e -> RunCtx EventX Config -> Options -> m Options
myAppMain exec run_ctx _opts = do
    heartbeat_h <- liftIO $ async (heartbeat exec run_ctx)
    ch          <- listenEventQueue run_ctx
    loop ch heartbeat_h
  where
    cleanup heartbeat_h = do
        liftIO $ cancel heartbeat_h

    loop ch heartbeat_h = do
        maybe_ev <- readEventQueue ch
        case maybe_ev of
            Nothing                             -> pure _opts
            Just (LogEventWithDetails _ _ _ ev) -> case ev of
                LogCmdAddAMFEv ev_amf -> do
                    case ev_amf of
                        (AMFEvSigReceived (UnixSignal sig)) -> do
                            if
                                | sig == Posix.sigHUP -> do
                                    loop ch heartbeat_h
                                | sig == Posix.sigINT -> do
                                    cleanup heartbeat_h
                                    pure _opts
                                | sig == Posix.sigTERM -> do
                                    cleanup heartbeat_h
                                    pure _opts
                                | otherwise -> loop ch heartbeat_h
                        _ -> loop ch heartbeat_h
                _ -> do
                    loop ch heartbeat_h

myAppFinish :: (AppConstraints m) => RunCtx e c -> v -> m ()
myAppFinish _run_ctx _ = do
    pass


--------------------------------------------------------------------------------

app :: (AppConstraints m, Executor e) => AppSpec m e EventX Options Config Options
app = AppSpec { appName    = "amf-daemon"
              , optionSpec = newOptSpec "amf-daemon example" optSpec
              , configSpec = newConfigSpec cfgParser
              , appSetup   = myAppSetup
              , appMain    = myAppMain
              , appEnd     = myAppFinish
              }

main :: IO ()
main = do
    runAppSpecAsDaemon app
