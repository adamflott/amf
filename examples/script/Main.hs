module Main where

import           AMF.Prelude.Script

import qualified Data.Aeson                    as Aeson
import qualified System.Posix                  as Posix
import qualified Data.YAML                     as YAML

import           Turtle.Prelude


type Ctx = RunCtx EventX Config Config Config

data Config = Config
    { i :: Maybe Int
    , s :: Maybe Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (Serialise, Aeson.ToJSON)


--------------------------------------------------------------------------------

instance FromEnv Config where
    fromEnv _ = Config <$> envMaybe "AMF_I" .!= Just 1 <*> envMaybe "AMF_S" .!= Just "str"


--------------------------------------------------------------------------------

optSpec :: OptionParser Config
optSpec = Config <$> optional optI <*> optional optS
  where
    optI = option auto (long "int" <> short 'i' <> value 1 <> showDefault <> help "Int")
    optS = strOption (long "str" <> short 's' <> metavar "STR" <> value "I'm a string!" <> showDefault <> help "String")


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
        _ -> Nothing

daemonEvLineFmt :: EventX -> LByteString
daemonEvLineFmt ev = evFmt ev <> "\n"
  where
    evFmt = \case
        EventConfig cfg -> "cfg:" <> show cfg

--------------------------------------------------------------------------------


instance YAML.FromYAML Config where
    parseYAML = YAML.withMap "Example Script Config" $ \m -> Config <$> m YAML..: "i" <*> m YAML..: "s"

cfgParser :: ConfigParser Config
cfgParser = yamlParser

--------------------------------------------------------------------------------

myAppSetup :: (AllAppConstraints m) => e -> Ctx -> Config -> m (Either ExitCode ())
myAppSetup _ run_ctx cfg = do
    addSignalHandler run_ctx [Posix.sigHUP, Posix.sigTERM, Posix.sigINT] sigHandler
    logEvent run_ctx LogLevelTerse (EventConfig cfg)
    pure (Right ())

myAppMain :: (AllAppConstraints m) => e -> Ctx -> Config -> () -> m ()
myAppMain _exec _run_ctx _opts _st = do
    pass


myAppFinish :: (AllAppConstraints m) => Ctx -> () -> m ()
myAppFinish _run_ctx _ = do
    pass

--------------------------------------------------------------------------------

app :: (AllAppConstraints m) => AppSpec m e EventX Config Config Config ()
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
