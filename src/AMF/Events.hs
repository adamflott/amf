{-# OPTIONS_GHC -fno-warn-orphans #-}

module AMF.Events where

import           Relude

-- base
import           Control.Concurrent             ( ThreadId                                                )

-- Hackage
import           Chronos
import qualified Data.Aeson                    as Aeson
import qualified System.Posix                  as Posix
import qualified Data.Text.Lazy                as LText
import           Codec.Serialise               as CBOR

-- local
import           AMF.Logging.Types
import           AMF.Logging.Types.Format

newtype UnixSignal = UnixSignal Posix.Signal
                   deriving stock (Generic, Show)
                   deriving anyclass (Serialise)

instance Aeson.ToJSON UnixSignal where
    toJSON (UnixSignal sig) = Aeson.Number (fromIntegral sig)

data AMFEvent
    = AMFEvStart Time
    | AMFEvStop
    | AMFEvSig UnixSignal
    deriving stock (Generic, Show)
    deriving anyclass (Serialise, Aeson.ToJSON)

deriving stock instance  Generic Time
deriving anyclass instance Serialise Time



fmtTime :: Time -> LByteString
fmtTime ts = encodeUtf8 $ encode_DmyHMS SubsecondPrecisionAuto w3c (timeToDatetime ts)

truncateThreadId :: ThreadId -> LByteString
truncateThreadId tid = encodeUtf8 $ fromMaybe "" (LText.stripPrefix "ThreadId " (show tid))

instance Eventable AMFEvent where
    toFmt fmt _hn _ln ts (_pid, tid) _lvl ev = do
        let ts'  = fmtTime ts
            tid' = truncateThreadId tid
        case fmt of
            LogFormatLine -> case ev of
                AMFEvStart t -> Just (ts' <> " " <> tid' <> " A " <> fmtTime t)
                AMFEvStop    -> Just (ts' <> " " <> tid' <> " B " <> "\n")
                AMFEvSig sig -> Just (ts' <> " " <> tid' <> show sig)
            LogFormatJSON -> Just (Aeson.encode ev <> "\n")
            LogFormatCBOR -> Just (CBOR.serialise ev)
            LogFormatCSV  -> Nothing
