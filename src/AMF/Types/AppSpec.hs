module AMF.Types.AppSpec where

import           Relude

-- base
import           System.Exit                    ( ExitCode )

-- Hackage
import           Options.Applicative
import           Validation
import qualified Data.YAML                     as YAML
import qualified Toml                          as TOML
import qualified System.Envy                   as Envy

-- local
import           AMF.Types.Config
import           AMF.Types.RunCtx
import           AMF.Events
import           AMF.API


type EnvParser a = (Either String a)

type EnvSpec a = a


type OptionParser = Parser

data OptionSpec a = OptionSpec
    { _optSpecDesc :: Text
    , _optSpec     :: OptionParser a
    }


data AppSpec m e exec_ev ev env opts cfg a = AppSpec
    { appName    :: Text
    , envSpec    :: EnvSpec env
    , optionSpec :: OptionSpec opts
    , configSpec :: ConfigSpec cfg
    , appSetup   :: e -> RunCtx exec_ev ev env opts cfg -> opts -> m (Either ExitCode a)
    , appMain    :: e -> RunCtx exec_ev ev env opts cfg -> opts -> a -> m a
    , appEnd     :: RunCtx exec_ev ev env opts cfg -> a -> m ()
    }

--------------------------------------------------------------------------------


data NoEvent = NoEvent
    deriving stock (Generic, Show)

data NoEnv = NoEnv
    deriving stock Generic

data NoOpts = NoOpts
type NoConfig = ()




instance Eventable NoEvent where
    toFmt _ _ _ _ _ _ _ = Nothing


--------------------------------------------------------------------------------

newEnvSpec :: EnvSpec a
newEnvSpec = undefined

instance Envy.FromEnv NoEnv where
    fromEnv _ = pure NoEnv

--------------------------------------------------------------------------------

newOptSpec :: Text -> OptionParser a -> OptionSpec a
newOptSpec = OptionSpec

emptyOptSpec :: OptionSpec NoOpts
emptyOptSpec = OptionSpec "no options" (pure NoOpts)

parseOpts :: OptionSpec a -> IO a
parseOpts spec = customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) (appOpts (_optSpec spec) (_optSpecDesc spec))

appOpts :: OptionParser a -> Text -> ParserInfo a
appOpts optSpec desc = info (optSpec <**> helper) (fullDesc <> progDesc (toString desc))

--------------------------------------------------------------------------------

yamlParser :: YAML.FromYAML a => ConfigParser a
yamlParser =
    (ConfigParser "yaml" $ \v -> do
        let r = YAML.decode1 v
        case r of
            Left  err -> Left (ConfigParserErr err)
            Right v'  -> Right v'
    )

tomlParser :: TOML.TomlCodec a -> ConfigParser a
tomlParser p =
    (ConfigParser "toml" $ \v -> do
        let r = TOML.decode p (decodeUtf8 v)
        case r of
            Left  err -> Left (ConfigParserErr err)
            Right v'  -> Right v'
    )

newConfigSpec :: ConfigParser a -> ConfigValidator a -> ConfigSpec a
newConfigSpec = ConfigSpec

noValidateConfig :: ConfigValidator a
noValidateConfig = ConfigValidator Validation.Success

--------------------------------------------------------------------------------

noAppSetup _ _run_ctx _cfg = do
    pure (Right ())

noAppFinish :: (AllAppConstraints m) => RunCtx exec_ev ev env opts cfg -> () -> m ()
noAppFinish _run_ctx _ = do
    pass

defaultAppSpec :: AppSpec m e exec_ev ev NoEnv NoOpts NoConfig ()
defaultAppSpec = AppSpec { appName    = "untitled"
                         , envSpec    = newEnvSpec
                         , optionSpec = emptyOptSpec
                         , configSpec = newConfigSpec (ConfigParser "no config" (\_ -> Right ())) noValidateConfig
                         , appSetup   = undefined
                         , appMain    = undefined
                         , appEnd     = undefined
                         }
