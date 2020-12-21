{-# OPTIONS_GHC -fno-warn-orphans #-}

module AMF.Events
    ( Eventable(..)
    , AMFEvent(..)
    , Phase(..)

    , fmtTime
    , truncateThreadId
    , timespanToString
    , defaultLinePrefixFormatter
    , (<+>)
    ) where

import           Relude

-- base
import           Control.Concurrent             ( ThreadId )
import           Data.Version

-- Hackage
import           Chronos
import           Codec.Serialise               as CBOR
import qualified Data.Aeson                    as Aeson
import qualified Data.Text.Lazy                as LText

-- local
import           AMF.Logging.Types.Format
import           AMF.Logging.Types.Level
import           AMF.Types.Common


class Aeson.ToJSON a => Eventable a where
  toFmt :: LogFormat -> HostName -> UserName -> Time -> (ProcessId, ThreadId) -> LogLevel -> a -> Maybe LByteString


data Phase
    = Setup
    | Main
    | Finish
    deriving stock (Bounded, Enum, Generic, Show)
    deriving anyclass (Serialise, Aeson.ToJSON)

data AMFEvent
    = AMFEvStart Version
    | AMFEvStop Version Timespan

    | AMFEvPhase Phase

    | AMFEvSigReceived UnixSignal
    | AMFEvSigSent UnixSignal

    | AMFEvLogEventOpen
    | AMFEvLogEventClose
    | AMFEvLogEventCmdRotate
    | AMFEvLogEventRotated
    | AMFEvLogEventRotating

    | AMFEvFSEntry Text Text
    deriving stock (Generic, Show)
    deriving anyclass (Serialise, Aeson.ToJSON)

deriving stock instance  Generic Timespan
deriving anyclass instance Serialise Timespan

deriving stock instance  Generic Time
deriving anyclass instance Serialise Time

{-

instance ToJSON LogEventInternal where
    toJSON = \case
        LogEventOpen      -> object [("event", "open")]
        LogEventClose     -> object [("event", "close")]
        LogEventCmdRotate -> object [("event", "system"), ("cmd", "rotate")]
        LogEventRotating  -> object [("event", "system"), ("cmd", "rotating")]
        LogEventRotated   -> object [("event", "system")]

-}



instance Eventable AMFEvent where
    toFmt fmt hn ln ts (pid, tid) lvl ev = case fmt of
        LogFormatLine -> Just (defaultLinePrefixFormatter hn ln ts (pid, tid) lvl <+> amfEvLineFmt ev)
        LogFormatJSON -> Just (Aeson.encode ev <> "\n")
        LogFormatCBOR -> Just (CBOR.serialise ev)
        LogFormatCSV  -> Nothing

amfEvLineFmt :: AMFEvent -> LByteString
amfEvLineFmt ev = "amf:" <> evFmt ev <> "\n"
  where
    evFmt = \case
        AMFEvStart vers        -> "start version:" <> encodeUtf8 (Data.Version.showVersion vers)
        AMFEvStop vers uptime  -> "stop version:" <> encodeUtf8 (Data.Version.showVersion vers) <> " uptime:" <> timespanToString uptime

        AMFEvPhase phase -> "phase:" <> show phase

        AMFEvSigReceived sig   -> "signal.received:" <> show sig
        AMFEvSigSent sig   -> "signal.sent:" <> show sig

        AMFEvLogEventOpen      -> "log.open"
        AMFEvLogEventClose     -> "log.close"
        AMFEvLogEventCmdRotate -> "log.rotate"
        AMFEvLogEventRotating  -> "log.rotating"
        AMFEvLogEventRotated   -> "log.rotated"
        AMFEvFSEntry name path -> "fs.path name:" <> show name <+> "path:" <> show path

--------------------------------------------------------------------------------


toSLBS :: ConvertUtf8 a LByteString => a -> LByteString
toSLBS = encodeUtf8

-- toLBS :: (Show a) => a -> LByteString
--toLBS = encodeUtf8 . show

(<+>) :: LByteString -> LByteString -> LByteString
x <+> y = x <> " " <> y

infixr 6 <+>

--(<++>) :: (Show a, Show b) => a -> b -> LByteString
--x <++> y = toLBS x <+> toLBS y

--infixr 6 <++>

fmtTime :: Time -> LByteString
fmtTime = encodeUtf8 . encode_YmdHMS SubsecondPrecisionAuto w3c . timeToDatetime

-- base does not expose the underlying thread id, so we have to strip the unnecessary prefix
truncateThreadId :: ThreadId -> LByteString
truncateThreadId tid = encodeUtf8 $ fromMaybe "" (LText.stripPrefix "ThreadId " (show tid))

timespanToString :: Timespan -> LByteString
timespanToString ts = encodeUtf8 (encodeTimespan SubsecondPrecisionAuto ts) <> "s"


defaultLinePrefixFormatter :: HostName -> UserName -> Time -> (ProcessId, ThreadId) -> LogLevel -> LByteString
defaultLinePrefixFormatter (HostName hn) (UserName ln) ts (ProcessId pid, tid) lvl =
    fmtTime ts <+> toSLBS hn <+> toSLBS ln <+> (show pid <> "/" <> truncateThreadId tid) <+> fmtLevel lvl
