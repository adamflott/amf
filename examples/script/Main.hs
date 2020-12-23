module Main where

import           AMF.Prelude.Script

import           Codec.Serialise               as CBOR
import qualified Data.Aeson                    as Aeson
import qualified System.Posix                  as Posix
import qualified Data.YAML                     as YAML

import           Turtle.Prelude

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
cfgParser = yamlParser

--------------------------------------------------------------------------------

myAppSetup :: (AppConstraints m, Executor e) => e -> Ctx -> Config -> m (Either ExitCode ())
myAppSetup _ run_ctx _ = do
    addSignalHandler run_ctx [Posix.sigHUP, Posix.sigTERM, Posix.sigINT] sigHandler
    pure (Right ())

myAppMain :: (AppConstraints m, Executor e) => e -> Ctx -> Config -> () -> m ()
myAppMain exec run_ctx _opts st = do
    pass


myAppFinish :: (AppConstraints m) => Ctx -> () -> m ()
myAppFinish _run_ctx _ = do
    pass

--------------------------------------------------------------------------------

app :: (AppConstraints m, Executor e) => AppSpec m e EventX Config Config Config ()
app = AppSpec { appName    = "amf-script"
              , envSpec    = newEnvSpec
              , optionSpec = newOptSpec "amf-script example" optSpec
              , configSpec = newConfigSpec cfgParser
              , appSetup   = myAppSetup
              , appMain    = myAppMain
              , appEnd     = myAppFinish
              }

main :: IO ()
main = runAppSpecAsScript app
