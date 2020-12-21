module AMF.API where

import           Relude

-- Hackage
import           BroadcastChan
import           Control.Lens
import           Path
import           Chronos
import qualified System.Posix                  as Posix

-- local
import           AMF.Types.Common
import           AMF.Events
import           AMF.Logging
import           AMF.Logging.Types
import           AMF.Logging.Types.Console
import           AMF.Logging.Types.Level
import           AMF.Logging.Types.OutputsInterface
import           AMF.Types.Executor
import           AMF.Types.SystemInfo


data ConfigEvent
    = ConfigEventModified
    | ConfigEventDeleted
    | ConfigEventAdded

makeClassy ''ConfigEvent


class (Monad m, MonadIO m) => MonadUnixSignals m where
  addSignalHandler :: RunCtx e -> [Posix.Signal] -> (RunCtx e -> Posix.Signal -> IO ()) -> m ()
  default addSignalHandler :: (MonadTrans t, MonadUnixSignals m', m ~ t m') => RunCtx e -> [Posix.Signal] -> (RunCtx e -> Posix.Signal -> IO ()) -> m ()
  addSignalHandler run_ctx sigs fn = lift (addSignalHandler run_ctx sigs fn)

class (Monad m, MonadEventLogger m) => MonadUnixSignalsRaise m where
  raiseSignal :: RunCtx e -> Posix.Signal -> m ()
  default raiseSignal :: (MonadTrans t, MonadUnixSignalsRaise m', m ~ t m') => RunCtx e -> Posix.Signal -> m ()
  raiseSignal run_ctx sig = lift (raiseSignal run_ctx sig)


class Monad m => MonadReadConfig m where
  getXConfig :: m a

class Monad m => MonadReactConfig m where
  addReactConfigHandler :: Path b File -> (RunCtx e -> ConfigEvent -> m ()) -> m ()


class Monad m => MonadEventLogger m where
  logEvent :: RunCtx e -> LogLevel -> e -> m ()
  logAMFEvent :: RunCtx e -> LogLevel -> AMFEvent -> m ()

class (Monad m) => MonadLoggerConsoleAdd m where
  addLogger :: LoggerCtx a -> OutputHandle LogOutputConsole -> m ()


class Monad m => MonadTime m where
  getNow :: m Time
  default getNow :: (MonadTrans t, MonadTime m', m ~ t m') => m Time
  getNow = lift getNow


class Monad m => MonadUser m where
  getUser :: m User


class Monad m => MonadRunCtxGet m where
  getRunCtx :: m e


class Monad m => MonadEventQueueRead m where
  readEventQueue :: BroadcastChan Out (LogEventWithDetails a) -> m (Maybe (LogEventWithDetails a))

  default readEventQueue :: (MonadTrans t, MonadEventQueueRead m', m ~ t m') => BroadcastChan Out (LogEventWithDetails a) -> m (Maybe (LogEventWithDetails a))
  readEventQueue = lift . readEventQueue

class Monad m => MonadEventQueueGetOut m where
  getAMFEventQueueOut :: m (BroadcastChan Out a)

class (Monad m, Eventable e) => MonadEventQueueWrite m e where
  writeEventQueue :: BroadcastChan In e -> e -> m Bool

class Monad m => MonadEventQueueClose m where
  closeEventQueue :: BroadcastChan In a -> m Bool

class Monad m => MonadEventQueueListen m where
  listenEventQueue :: RunCtx e -> m (BroadcastChan Out (LogEventWithDetails (LogCmd e)))

--------------------------------------------------------------------------------

instance MonadTime IO where
    getNow = now

instance MonadUnixSignals IO where
    addSignalHandler run_ctx sigs fn = do
        liftIO $ do
            mapM_ (\sig -> Posix.installHandler sig (runSigHandler fn run_ctx) Nothing) sigs

runSigHandler :: (RunCtx e -> Posix.Signal -> IO ()) -> RunCtx e -> Posix.Handler
runSigHandler fn run_ctx = Posix.CatchInfo \(Posix.SignalInfo sig _errno _si) -> do
    AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvSigReceived (UnixSignal sig))
    fn run_ctx sig

instance MonadEventQueueRead IO where
    readEventQueue ch = liftIO (BroadcastChan.readBChan ch)

instance MonadEventQueueListen IO where
  listenEventQueue run_ctx = liftIO (newBChanListener (run_ctx ^. runCtxLogger . loggingCtxIntInEv))

instance Eventable a => MonadEventQueueWrite IO a where
    writeEventQueue ch msg = liftIO (BroadcastChan.writeBChan ch msg)

instance MonadEventQueueClose IO where
    closeEventQueue ch = liftIO (closeBChan ch)

instance MonadEventLogger IO where
    logEvent run_ctx lvl ev = liftIO $ AMF.Logging.logEvent (run_ctx ^. runCtxLogger) lvl ev

    logAMFEvent run_ctx lvl ev = liftIO $ AMF.Logging.logAMFEvent (run_ctx ^. runCtxLogger) lvl ev

instance MonadLoggerConsoleAdd IO where
    addLogger log_ctx out = addConsoleLogger log_ctx out

instance MonadUnixSignalsRaise IO where
    raiseSignal run_ctx sig = do
      AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvSigSent (UnixSignal sig))
      liftIO (Posix.raiseSignal sig)

logExecutorFsEntries :: (MonadEventLogger m, Executor a) => RunCtx e -> a -> m ()
logExecutorFsEntries run_ctx e = do
  AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvFSEntry "root" (f (fsDirRoot e)))
  AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvFSEntry "metadata" (f (fsDirMetadata e)))
  AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvFSEntry "logs" (f (fsDirLogs e)))
  AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvFSEntry "app_info" (f (fsFileAppInfo e)))
  AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvFSEntry "cache" (f (fsDirCache e)))
  where
    f :: Maybe (Path b t) -> Text
    f = maybe "N/A" (toText . toFilePath)
