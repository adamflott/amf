module AMF.Logging.Outputs.Console
    ( newConsoleOutput
    ) where

-- prelude
import           Relude

-- base

-- Hackage
import           Control.Monad.Catch            ( MonadMask )

-- local
import           AMF.Logging.Types.Format
import           AMF.Logging.Types.Level
import           AMF.Logging.Types.Console
import           AMF.Logging.Types.OutputsInterface


newConsoleOutput :: (MonadIO m, MonadMask m) => LogLevel -> LogFormat -> StdOutOrErr -> m (Either Text (OutputHandle LogOutputConsole))
newConsoleOutput lvl fmt soe = case soe of
    LogOutputStdOut -> openOutput (LogOutputConsole lvl fmt LogOutputStdOut stdout)
    LogOutputStdErr -> openOutput (LogOutputConsole lvl fmt LogOutputStdErr stderr)
