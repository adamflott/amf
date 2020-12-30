module AMF.Config.Logging where

import           Relude

-- Hackage
import           Data.YAML

-- local
import           AMF.Logging.Types.Console
import           AMF.Logging.Types.Level
import           AMF.Logging.Types.Format


data ConfLoggingOutputs = ConfLogOutputConsole LogLevel LogFormat StdOutOrErr
    deriving stock (Generic, Show)

instance FromYAML ConfLoggingOutputs where
    parseYAML = withMap "log.outputs" $ \m -> do
        ty <- m .: "type"
        parseLogOutputType ty m
      where
        parseLogOutputType ty m = case (ty :: Text) of
            "console" -> do
                ConfLogOutputConsole <$> m .: "level" <*> m .: "format" <*> m .: "stdout_or_stderr"
            other -> fail (toString other <> " is not supported")


data ConfLogging = ConfLogging
    { enabled :: Bool
    , outputs :: [ConfLoggingOutputs]
    }
    deriving stock (Generic, Show)
--                 deriving anyclass (Serialise)


instance FromYAML ConfLogging where
    parseYAML = withMap "logging" $ \m -> ConfLogging <$> m .: "enabled" <*> m .: "outputs"

