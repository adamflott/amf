module Main where

-- prelude
import           Relude

-- base
import           Control.Concurrent             ( ThreadId, threadDelay )

-- Hackage
import Chronos
import           Data.Aeson as Aeson
import Path
import Codec.Serialise as CBOR
import qualified Data.Text.Lazy as LText

-- local
import           AMF.Logging
import           AMF.Logging.Outputs.Console
import           AMF.Logging.Outputs.File
import           AMF.Logging.Types
import           AMF.Logging.Types.Console
import           AMF.Logging.Types.File
import           AMF.Logging.Types.Format
import           AMF.Logging.Types.Level


data EventX
    = EventA Text Text
    | EventB Int
    deriving stock (Generic, Show)
    deriving anyclass (Serialise, ToJSON)

fmtTime :: Time -> LByteString
fmtTime ts = encodeUtf8 $ encode_DmyHMS SubsecondPrecisionAuto w3c (timeToDatetime ts)

truncateThreadId :: ThreadId -> LByteString
truncateThreadId tid = encodeUtf8 $ fromMaybe "" (LText.stripPrefix "ThreadId " (show tid))

instance Eventable EventX where
    toFmt fmt hn ln ts (pid, tid) lvl ev = do
      let ts' = fmtTime ts
          tid' = truncateThreadId tid
      case fmt of
        LogFormatLine -> case ev of
            EventA a b  -> Just (ts' <> " " <> tid' <> " A " <> encodeUtf8 a <> " " <> encodeUtf8 b <> "\n")
            EventB i -> Just (ts' <> " " <> tid' <> " B " <> show i <> "\n")
        LogFormatJSON -> Just (Aeson.encode ev <> "\n")
        LogFormatCBOR -> Just (CBOR.serialise ev)
        LogFormatCSV -> Nothing

main :: IO ()
main = do
    let out_stdout = newConsoleOutput LogLevelAll LogFormatLine LogOutputStdOut
        out_stderr = newConsoleOutput LogLevelAll LogFormatJSON LogOutputStdErr
        f_out = newFileUTF8Output LogLevelAll LogFormatLine $(mkAbsFile "/tmp/a.log") Append

    let los = LogOutputs [out_stdout, out_stderr] [f_out]

    cfg <- newConfig los

    (ctx :: LoggerCtx EventX) <- newLoggingCtx cfg

    h <- startLogger ctx

    logEvent ctx LogLevelAll (EventA "たろう" "Иван")
    logEvent ctx LogLevelAll (EventB 1)
    logEvent ctx LogLevelTerse (EventB 2)

    threadDelay 1000000

    stopLogger ctx h

    pass
