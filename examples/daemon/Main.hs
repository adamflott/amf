module Main where

-- prelude
import           Relude

-- base
import           Control.Concurrent             ( threadDelay )
import           System.Exit

-- Hackage
import           Codec.Serialise               as CBOR
import           Control.Concurrent.Async
import           Control.Lens            hiding ( (.=) )
import           Control.Monad.Catch            ( MonadMask )
import           Options.Applicative
import qualified Data.Aeson                    as Aeson
import qualified System.Posix                  as Posix
import qualified Data.YAML                     as YAML
import           Path
import qualified Toml                          as TOML
import           System.Envy
import           Validation

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

type Ctx = RunCtx EventX Config Config Config

data Config = Config
    { i :: Int
    , s :: Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (Serialise, Aeson.ToJSON)


--------------------------------------------------------------------------------

instance FromEnv Config where
    fromEnv _ = Config <$> envMaybe "AMF_I" .!= 1 <*> envMaybe "AMF_S" .!= "str"


--------------------------------------------------------------------------------

optSpec :: OptionParser Config
optSpec = Config <$> optI <*> optS
  where
    optI = option auto (long "int" <> short 'i' <> help "Int")
    optS = strOption (long "str" <> short 's' <> metavar "STR" <> value "" <> showDefault <> help "")


--------------------------------------------------------------------------------

sigHandler :: Ctx -> Posix.Signal -> IO ()
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


instance YAML.FromYAML Config where
    parseYAML = YAML.withMap "Example Daemon Config" $ \m -> Config <$> m YAML..: "i" <*> m YAML..: "s"




cfgParser :: ConfigParser Config
cfgParser = parser
  where
    parser = tomlParser cfgTOMLCodec
    cfgTOMLCodec :: TOML.TomlCodec Config
    cfgTOMLCodec = Config <$> TOML.int "i" TOML..= i <*> TOML.text "s" TOML..= s
    -- or use YAML
    -- parser = yamlParser

--------------------------------------------------------------------------------

data MyState = MyState Int

--------------------------------------------------------------------------------

getAppConfigFilepath :: Executor a => a -> RunCtx ev env opts cfg -> Text
getAppConfigFilepath exec run_ctx = do
    let d        = fsDirJoin (fsDirRoot exec) [fsDirMetadata exec]
        fn       = configFilename run_ctx (run_ctx ^. runCtxAppName)
        maybe_fp = fsFileJoin d fn

    maybe "?" (toText . toFilePath) maybe_fp

myAppSetup :: (AppConstraints m, Executor e) => e -> Ctx -> Config -> m (Either ExitCode MyState)
myAppSetup exec run_ctx opts = do

    -- setConfigBlockingReadAndParseFor run_ctx (configFile opts)

    let app_cfg_fp = getAppConfigFilepath exec run_ctx
    maybe_cfg <- getConfig run_ctx app_cfg_fp

    whenJust maybe_cfg $ \cfg -> do
        logEvent run_ctx LogLevelTerse (EventConfig cfg)

    addSignalHandler run_ctx [Posix.sigHUP, Posix.sigTERM, Posix.sigINT] sigHandler
    raiseSignal run_ctx Posix.sigHUP

    pure (Right (MyState 1))


heartbeat :: (MonadIO m, MonadEventLogger m, MonadConfigGet m, Executor e) => e -> Ctx -> m ()
heartbeat exec run_ctx = do
    liftIO $ threadDelay (10 * 1000000)

    let app_cfg_fp = getAppConfigFilepath exec run_ctx
    maybe_cfg <- getConfig run_ctx app_cfg_fp

    whenJust maybe_cfg $ \cfg -> do
        logEvent run_ctx LogLevelTerse (EventConfig cfg)

    heartbeat exec run_ctx

myAppMain :: (AppConstraints m, Executor e) => e -> Ctx -> Config -> MyState -> m MyState
myAppMain exec run_ctx _opts st = do
    heartbeat_h <- liftIO $ async (heartbeat exec run_ctx)
    ch          <- listenEventQueue run_ctx
    loop ch heartbeat_h
  where
    cleanup heartbeat_h = do
        liftIO $ cancel heartbeat_h

    loop ch heartbeat_h = do
        maybe_ev <- readEventQueue ch
        case maybe_ev of
            Nothing                             -> pure st
            Just (LogEventWithDetails _ _ _ ev) -> case ev of
                LogCmdAddAMFEv ev_amf -> do
                    case ev_amf of
                        (AMFEvSigReceived (UnixSignal sig)) -> do
                            if
                                | sig == Posix.sigHUP -> do
                                    loop ch heartbeat_h
                                | sig == Posix.sigINT -> do
                                    cleanup heartbeat_h
                                    pure st
                                | sig == Posix.sigTERM -> do
                                    cleanup heartbeat_h
                                    pure st
                                | otherwise -> loop ch heartbeat_h
                        _ -> loop ch heartbeat_h
                _ -> do
                    loop ch heartbeat_h

myAppFinish :: (AppConstraints m) => Ctx -> MyState -> m ()
myAppFinish _run_ctx _ = do
    pass


cfgValidator :: ConfigValidator Config
cfgValidator = ConfigValidator \v -> Validation.Success v

--------------------------------------------------------------------------------

app :: (AppConstraints m, Executor e) => AppSpec m e EventX Config Config Config MyState
app = AppSpec { appName    = "amf-daemon"
              , envSpec    = newEnvSpec
              , optionSpec = newOptSpec "amf-daemon example" optSpec
              , configSpec = newConfigSpec cfgParser cfgValidator
              , appSetup   = myAppSetup
              , appMain    = myAppMain
              , appEnd     = myAppFinish
              }

main :: IO ()
main = runAppSpecAsDaemon app
