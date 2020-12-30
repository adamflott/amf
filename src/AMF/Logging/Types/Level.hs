module AMF.Logging.Types.Level
    ( LogLevel(..)
    , fmtLevel
    ) where

import           Relude

-- Hackage
import           Data.YAML
import           Codec.Serialise               as CBOR

data LogLevel
    = LogLevelTerse
    | LogLevelVerbose
    | LogLevelAlways
    | LogLevelAll
    deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)
    deriving anyclass (Serialise)

levels :: String
levels = intercalate ", " (fmap show [minBound :: LogLevel .. maxBound])

instance FromYAML LogLevel where
    parseYAML = withStr "log.level" $ \case
        "terse"   -> pure LogLevelTerse
        "verbose" -> pure LogLevelVerbose
        "all"     -> pure LogLevelAll
        other     -> fail (toString other <> " is not a recognized levels: " <> levels)

fmtLevel :: IsString a => LogLevel -> a
fmtLevel = \case
    LogLevelTerse   -> "T"
    LogLevelVerbose -> "V"
    LogLevelAlways  -> "*"
    LogLevelAll     -> "A"
