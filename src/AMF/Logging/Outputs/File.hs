module AMF.Logging.Outputs.File
    (   newFileUTF8Output)
where

-- prelude
import           Relude ()

-- base

-- Hackage
import           Path

-- local
import           AMF.Logging.Types.Level
import           AMF.Logging.Types.Format
import           AMF.Logging.Types.File


newFileUTF8Output :: LogLevel -> LogFormat -> Path Abs File -> LogOutputFileWriteMode -> LogOutputFile
newFileUTF8Output = LogOutputFile
