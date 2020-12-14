module AMF.Logging.Types
    ( LoggerConfig(..)
    , DynamicLoggerConfig(..)
    , LoggerCtx(..)
    , LoggerHandle
    , LogCmd(..)
    , HostName(..)
    , UserName(..)
    , ProcessId(..)

    -- * Outputs
    , LogOutputs(..)
    , LogEventWithDetails(..)
 --   , LogEvent(..)
    , LogEventInternal(..)
    , Eventable(..)
    , runLoggerT

    -- * Lenses
    , loggingCtxHostName
    , loggingCtxProcessId
    , loggingCtxUserName
    , loggingCtxCfg
    , loggingCtxIntInEv
    , loggingCtxIntOutEv
    , loggingCtxExtInEv
    , loggingCtxExtOutEv
    , loggingCtxOutputHandlesConsole
    , loggingCtxOutputHandlesFile
    , loggingCtxOutputs
    ) where

-- prelude
import           Relude

-- base
import           Control.Concurrent             ( ThreadId )
-- import System.IO (Handle)

-- Hackage
--import Control.Monad.Trans.Control (MonadBaseControl)
import           BroadcastChan
import           Chronos
import           Codec.Serialise               as CBOR
import           Control.Concurrent.Async.Lifted
                                                ( Async )
import           Control.Lens
import           Control.Monad.Base             ( MonadBase )
import           Control.Monad.Catch            ( MonadThrow
                                                , MonadCatch
                                                , MonadMask
                                                )
import           Control.Monad.Trans.Resource   ( MonadResource )
import           Data.Aeson                    as Aeson
import qualified System.Posix.Types            as POSIX

-- local
import           AMF.Logging.Types.Console
import           AMF.Logging.Types.File
import           AMF.Logging.Types.Format
import           AMF.Logging.Types.Level
import           AMF.Logging.Types.Outputs
import           AMF.Logging.Types.OutputsInterface


newtype HostName = HostName Text
                 deriving stock (Generic, Show)
                 deriving newtype IsString

newtype UserName = UserName Text
                 deriving stock (Generic, Show)
                 deriving newtype IsString

newtype ProcessId = ProcessId POSIX.ProcessID
                  deriving stock (Generic, Show)


class ToJSON a => Eventable a where
  toFmt :: LogFormat -> HostName -> UserName -> Time -> (ProcessId, ThreadId) -> LogLevel -> a -> Maybe LByteString

--instance CBOR.Serialise (Path Abs File)
--instance CBOR.Serialise (Path Rel File)
--instance CBOR.Serialise (Path Abs Dir)
--instance CBOR.Serialise (Path Rel Dir)

data LogEventInternal
    = LogEventOpen
    | LogEventClose
    | LogEventCmdRotate
    | LogEventRotated
    | LogEventRotating
    deriving stock (Generic, Show)
    deriving anyclass (Serialise)


instance ToJSON LogEventInternal where
    toJSON = \case
        LogEventOpen      -> object [("event", "open")]
        LogEventClose     -> object [("event", "close")]
        LogEventCmdRotate -> object [("event", "system"), ("cmd", "rotate")]
        LogEventRotating  -> object [("event", "system"), ("cmd", "rotating")]
        LogEventRotated   -> object [("event", "system")]

instance Eventable LogEventInternal where
    toFmt fmt hn un ts (pid, tid) lvl ev = case fmt of
        LogFormatLine -> case ev of
            LogEventOpen      -> Just (show ts <> show tid <> " log.open\n")
            LogEventClose     -> Just (show ts <> show tid <> " log.close\n")
            LogEventCmdRotate -> Just (show ts <> show tid <> " log.rotate\n")
            LogEventRotating  -> Just (show ts <> show tid <> " log.rotating\n")
            LogEventRotated   -> Just (show ts <> show tid <> " log.rotated\n")
        LogFormatJSON -> Just (Aeson.encode ev <> "\n")
        LogFormatCBOR -> Just (CBOR.serialise ev)
        LogFormatCSV  -> Nothing


newtype LoggerConfig = LoggerConfig {
    _loggingCtxOutputs    :: TVar LogOutputs
    }

makeLenses ''LoggerConfig
newtype DynamicLoggerConfig = DynamicLoggerConfig (TVar LoggerConfig)


-- | Log event. Includes high resolution time with the thread id that generated the event.
data LogEventWithDetails ev = LogEventWithDetails
    { _ts  :: !Time
    , _tid :: !ThreadId
    , _lvl :: !LogLevel
    , _lev :: ev
    }
    deriving stock Show

-- | Controls how the logger process deals with a new channel item.
data LogCmd
    = LogCmdAdd !LogEventInternal -- ^ add log event
    | LogCmdRotate -- ^ rotate log file
    deriving stock Show

-- TODO
data LogStats = LogStats Int Int

-- | Logger context
data LoggerCtx ev = LoggerCtx
    { _loggingCtxHostName             :: !HostName
    , _loggingCtxUserName             :: !UserName
    , _loggingCtxProcessId            :: !ProcessId
    , _loggingCtxCfg                  :: !DynamicLoggerConfig
    , _loggingCtxIntInEv              :: !(BroadcastChan In (LogEventWithDetails LogCmd))
    , _loggingCtxIntOutEv             :: !(BroadcastChan Out (LogEventWithDetails LogCmd))
    , _loggingCtxExtInEv              :: !(BroadcastChan In (LogEventWithDetails ev))
    , _loggingCtxExtOutEv             :: !(BroadcastChan Out (LogEventWithDetails ev))
    , _loggingCtxOutputHandlesConsole :: TVar [OutputHandle LogOutputConsole]
    , _loggingCtxOutputHandlesFile    :: TVar [OutputHandle LogOutputFile]
    }

makeLenses ''LoggerCtx

-- | Logger thread handle.
type LoggerHandle = Async ()


-- | Logger Monad.
newtype LoggerT m ev a = LoggerT {
    runLoggerT :: ReaderT (LoggerCtx ev) m a
    }
    deriving newtype (Functor, Applicative, Monad)
    deriving newtype (MonadReader (LoggerCtx ev), MonadThrow, MonadCatch, MonadMask, MonadResource, MonadIO)

deriving newtype instance MonadBase IO m => MonadBase IO (LoggerT m ev)
--deriving newtype instance MonadBaseControl IO m => MonadBaseControl IO (LoggerT m o e a)
-- deriving newtype instance MonadError e m => MonadError e (LoggerT m)
