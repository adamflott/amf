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

    -- * Lenses
    , loggingCtxHostName
    , loggingCtxProcessId
    , loggingCtxUserName
    , loggingCtxCfg
    , loggingCtxIntInEv
    , loggingCtxIntOutEv
    , loggingCtxOutputHandlesConsole
    , loggingCtxOutputHandlesFile
    , loggingCtxOutputs
    ) where

-- prelude
import           Relude

-- base
import           Control.Concurrent             ( ThreadId )

-- Hackage
import           BroadcastChan
import           Chronos
import           Control.Concurrent.Async.Lifted
                                                ( Async )
import           Control.Lens
--import           Control.Monad.Base             ( MonadBase )
--import           Control.Monad.Catch            ( MonadThrow
--                                                , MonadCatch
--                                                , MonadMask
--                                                )
--import           Control.Monad.Trans.Resource   ( MonadResource )
import           Data.Aeson                    as Aeson

-- local
import           AMF.Events
import           AMF.Logging.Types.Console
import           AMF.Logging.Types.File
import           AMF.Logging.Types.Level
import           AMF.Logging.Types.Outputs
import           AMF.Logging.Types.OutputsInterface
import           AMF.Types.Common


newtype LoggerConfig = LoggerConfig {
    _loggingCtxOutputs    :: TVar LogOutputs
    }

makeLenses ''LoggerConfig
newtype DynamicLoggerConfig = DynamicLoggerConfig (TVar LoggerConfig)


-- | Log event. Includes high resolution time with the thread id that generated the event.
data LogEventWithDetails ev = LogEventWithDetails
    { _ts  :: !Time
    , _tid :: ThreadId
    , _lvl :: LogLevel
    , _lev :: ev
    }
    deriving stock (Generic, Show)

-- | Controls how the logger process deals with a new channel item.
data LogCmd exec_ev ev
    = LogCmdAddEv ev -- ^ add log event
    | LogCmdAddExecEv exec_ev
    | LogCmdAddAMFEv AMFEvent -- ^ add log event
    | LogCmdRotate -- ^ rotate log file
    deriving stock (Generic, Show)

instance (ToJSON exec_ev, ToJSON ev) => ToJSON (LogCmd exec_ev ev)

instance (Eventable exec_ev, Eventable ev) => Eventable (LogCmd exec_ev ev) where
    toFmt fmt hn ln ts (pid, tid) lvl ev = case ev of
        LogCmdAddEv     app_ev  -> toFmt fmt hn ln ts (pid, tid) lvl app_ev
        LogCmdAddExecEv exec_ev -> toFmt fmt hn ln ts (pid, tid) lvl exec_ev
        LogCmdAddAMFEv  amf_ev  -> toFmt fmt hn ln ts (pid, tid) lvl amf_ev
        _ev                     -> toFmt fmt hn ln ts (pid, tid) lvl _ev


-- TODO
-- data LogStats = LogStats Int Int

-- | Logger context
data LoggerCtx exec_ev ev = LoggerCtx
    { _loggingCtxHostName             :: HostName
    , _loggingCtxUserName             :: UserName
    , _loggingCtxProcessId            :: ProcessId
    , _loggingCtxCfg                  :: DynamicLoggerConfig
    , _loggingCtxIntInEv              :: BroadcastChan In (LogEventWithDetails (LogCmd exec_ev ev))
    , _loggingCtxIntOutEv             :: BroadcastChan Out (LogEventWithDetails (LogCmd exec_ev ev))
    , _loggingCtxOutputHandlesConsole :: TVar [OutputHandle LogOutputConsole]
    , _loggingCtxOutputHandlesFile    :: TVar [OutputHandle LogOutputFile]
    }

makeLenses ''LoggerCtx

-- | Logger thread handle.
type LoggerHandle = Async ()


