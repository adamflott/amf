module AMF.Logging.Outputs.Console
    ( newConsoleOutput
    )
where

-- prelude
import Relude

-- base

-- Hackage

-- local
import           AMF.Logging.Types.Format
import           AMF.Logging.Types.Level
import           AMF.Logging.Types.Console


newConsoleOutput :: LogLevel -> LogFormat -> StdOutOrErr -> LogOutputConsole
newConsoleOutput lvl fmt soe =
  case soe of
    LogOutputStdOut -> LogOutputConsole lvl fmt LogOutputStdOut stdout
    LogOutputStdErr -> LogOutputConsole lvl fmt LogOutputStdErr stderr
