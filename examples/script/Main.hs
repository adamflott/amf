module Main where

import           AMF.Prelude.Script      hiding ( (</>) )

import qualified Data.Aeson                    as Aeson
import qualified System.Posix                  as Posix
import qualified Data.YAML                     as YAML

import           Turtle                         ( (</>)
                                                , view
                                                )
import           Turtle.Prelude


type AppCtx = RunCtx AppEvent AppConfig AppConfig AppConfig

data AppConfig = AppConfig
    { i :: Maybe Int
    , s :: Maybe Text
    }
    deriving stock (Generic, Show)
    deriving anyclass (Serialise, Aeson.ToJSON)


--------------------------------------------------------------------------------

instance FromEnv AppConfig where
    fromEnv _ = AppConfig <$> envMaybe "AMF_I" .!= Just 1 <*> envMaybe "AMF_S" .!= Just "str"


--------------------------------------------------------------------------------

optSpec :: OptionParser AppConfig
optSpec = AppConfig <$> optional optI <*> optional optS
  where
    optI = option auto (long "int" <> short 'i' <> value 1 <> showDefault <> help "Int")
    optS = strOption (long "str" <> short 's' <> metavar "STR" <> value "I'm a string!" <> showDefault <> help "String")


--------------------------------------------------------------------------------

sigHandler :: AppCtx -> Posix.Signal -> IO ()
sigHandler _run_ctx _sig = do
    pass

--------------------------------------------------------------------------------

data AppEvent
    = EventConfig AppConfig
    | EventMkTree String
    deriving stock (Generic, Show)
    deriving anyclass (Serialise, Aeson.ToJSON)



instance Eventable AppEvent where
    toFmt fmt hn ln ts (pid, tid) lvl ev = case fmt of
        LogFormatLine -> Just (defaultLinePrefixFormatter hn ln ts (pid, tid) lvl <+> daemonEvLineFmt ev)
        _             -> Nothing

daemonEvLineFmt :: AppEvent -> LByteString
daemonEvLineFmt ev = evFmt ev <> "\n"
  where
    evFmt = \case
        EventConfig cfg -> "cfg:" <> show cfg
        EventMkTree dir -> "mktree:" <> show dir

--------------------------------------------------------------------------------


instance YAML.FromYAML AppConfig where
    parseYAML = YAML.withMap "Example Script Config" $ \m -> AppConfig <$> m YAML..: "i" <*> m YAML..: "s"

cfgParser :: ConfigParser AppConfig
cfgParser = yamlParser

--------------------------------------------------------------------------------

myAppSetup :: (AllAppConstraints m) => e -> AppCtx -> AppConfig -> m (Either ExitCode ())
myAppSetup _ run_ctx cfg = do
    addSignalHandler run_ctx [Posix.sigHUP, Posix.sigTERM, Posix.sigINT] sigHandler
    logEvent run_ctx LogLevelTerse (EventConfig cfg)
    pure (Right ())

myAppMain :: (AllAppConstraints m) => e -> AppCtx -> AppConfig -> () -> m ()
myAppMain _exec run_ctx _opts _st = do

    -- sh like operations and logging
    let root = "/tmp" </> "amf" </> "logs"
    logEvent run_ctx LogLevelTerse (EventMkTree (show root))
    mktree root

    -- run tasks in parallel
    view (parallel [sleep 3 >> date, date, date])

    pass


myAppFinish :: (AllAppConstraints m) => AppCtx -> () -> m ()
myAppFinish _run_ctx _ = do
    pass

--------------------------------------------------------------------------------

app :: (AllAppConstraints m) => AppSpec m e AppEvent AppConfig AppConfig AppConfig ()
app = AppSpec { appName    = "amf-script"
              , envSpec    = newEnvSpec
              , optionSpec = newOptSpec "amf-script example" optSpec
              , configSpec = newConfigSpec cfgParser noValidateConfig
              , appSetup   = myAppSetup
              , appMain    = myAppMain
              , appEnd     = myAppFinish
              }

main :: IO ()
main = runAppSpecAsScript app
