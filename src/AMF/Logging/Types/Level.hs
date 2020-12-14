module AMF.Logging.Types.Level
    ( LogLevel(..)
    , fmtLevel
    )
where

import           Relude


data LogLevel
    = LogLevelTerse
    | LogLevelVerbose
    | LogLevelAll
    deriving stock (Bounded, Enum, Eq, Generic, Ord, Show)

fmtLevel :: IsString a => LogLevel -> a
fmtLevel = \case
    LogLevelTerse -> "T"
    LogLevelVerbose -> "V"
    LogLevelAll -> "A"
