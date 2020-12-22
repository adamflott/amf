module AMF.Types.AppSpec where

import           Relude

-- base
import           System.Exit                    ( ExitCode )

-- Hackage
import           Control.Monad.Catch            ( MonadMask )
import           Options.Applicative
import qualified Data.YAML                     as YAML

-- local
import           AMF.Types.Config
import           AMF.Types.RunCtx


type OptionParser = Parser

data OptionSpec a = OptionSpec
    { _optSpecDesc :: Text
    , _optSpec     :: OptionParser a
    }


data AppSpec m e ev opt cfg a = AppSpec
    { appName    :: Text
    , optionSpec :: OptionSpec opt
    , configSpec :: ConfigSpec cfg
    , appSetup   :: MonadMask m => (e -> RunCtx ev cfg -> opt -> m (Either ExitCode a))
    , appMain    :: e -> RunCtx ev cfg -> opt -> m a
    , appEnd     :: RunCtx ev cfg -> a -> m ()
    }



newOptSpec :: Text -> OptionParser a -> OptionSpec a
newOptSpec = OptionSpec

parseOpts :: OptionSpec a -> IO a
parseOpts spec = customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) (appOpts (_optSpec spec) (_optSpecDesc spec))

appOpts :: OptionParser a -> Text -> ParserInfo a
appOpts optSpec desc = info (optSpec <**> helper) (fullDesc <> progDesc (toString desc))

--------------------------------------------------------------------------------

yamlParser :: YAML.FromYAML a => ConfigParser a
yamlParser =
    (ConfigParserYAML $ \v -> do
        let r = YAML.decode1 v
        case r of
            Left  err -> Left (ConfigParseResultYAML err)
            Right v'  -> Right v'
    )


newConfigSpec :: ConfigParser a -> ConfigSpec a
newConfigSpec = ConfigSpec
