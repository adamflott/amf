module AMF.API where


import           Relude

-- base

-- Hackage
import           BroadcastChan
import           Chronos
import           Control.Lens
import           Control.Monad.Catch            ( MonadMask )
import qualified Data.Map.Strict               as Map
import qualified System.Posix                  as Posix

-- local
import           AMF.Events
import           AMF.Logging
import           AMF.Logging.Types
import           AMF.Logging.Types.Console
import           AMF.Logging.Types.Level
import           AMF.Logging.Types.OutputsInterface
import           AMF.Types.Common
import           AMF.Types.RunCtx
import           AMF.Types.SystemInfo


type AllAppConstraints m
    = ( MonadIO m
      , MonadMask m
      , MonadFail m
      , MonadTime m
      , MonadEventLogger m
      , MonadLoggerConsoleAdd m
      , MonadUnixSignals m
      , MonadUnixSignalsRaise m
      , MonadEventQueueRead m
      , MonadEventQueueListen m
      , MonadConfigGet m
      , MonadConfigChangeBlockingReact m
      )


class (Monad m, MonadIO m) => MonadUnixSignals m where
  addSignalHandler :: RunCtx exec_ev ev env opts cfg -> [Posix.Signal] -> (RunCtx exec_ev ev env opts cfg -> Posix.Signal -> IO ()) -> m ()
  default addSignalHandler :: (MonadTrans t, MonadUnixSignals m', m ~ t m') => RunCtx exec_ev ev env opts cfg -> [Posix.Signal] -> (RunCtx exec_ev ev env opts cfg -> Posix.Signal -> IO ()) -> m ()
  addSignalHandler run_ctx sigs fn = lift (addSignalHandler run_ctx sigs fn)

  watchSignal :: RunCtx exec_ev ev env opts cfg -> [Posix.Signal] -> m ()
  default watchSignal :: (MonadTrans t, MonadUnixSignals m', m ~ t m') => RunCtx exec_ev ev env opts cfg -> [Posix.Signal] -> m ()
  watchSignal run_ctx sigs = lift (watchSignal run_ctx sigs)

class (Monad m, MonadEventLogger m) => MonadUnixSignalsRaise m where
  raiseSignal :: RunCtx exec_ev ev env opts cfg -> Posix.Signal -> m ()
  default raiseSignal :: (MonadTrans t, MonadUnixSignalsRaise m', m ~ t m') => RunCtx exec_ev ev env opts cfg -> Posix.Signal -> m ()
  raiseSignal run_ctx sig = lift (raiseSignal run_ctx sig)


class Monad m => MonadConfigGet m where
  getConfig :: RunCtx exec_ev ev env opts cfg -> Text -> m (Maybe cfg)
  default getConfig :: (MonadTrans t, MonadConfigGet m', m ~ t m') => RunCtx exec_ev ev env opts cfg -> Text -> m (Maybe cfg)
  getConfig run_ctx name = lift (getConfig run_ctx name)

class Monad m => MonadConfigChangeBlockingReact m where
  setConfigDefault :: RunCtx exec_ev ev env opts cfg -> Maybe c -> m ()
  setConfigBlockingReadAndParseFor :: RunCtx exec_ev ev env opts cfg -> Text -> m ()

  --addReactConfigHandler :: RunCtx e c -> Text -> (RunCtx e c -> FSEvent -> IO ()) -> m ()
  --default addReactConfigHandler :: (MonadTrans t, MonadConfigChangeReact m', m ~ t m') => RunCtx e c -> Text -> (RunCtx e c -> FSEvent -> IO ()) -> m ()
  --addReactConfigHandler run_ctx fn f = lift (addReactConfigHandler run_ctx fn f)


class Monad m => MonadEventLogger m where
  logEvent :: LoggerCtx exec_ev ev -> LogLevel -> ev -> m ()
  logAMFEvent :: LoggerCtx exec_ev ev -> LogLevel -> AMFEvent -> m ()
  logExecEvent :: LoggerCtx exec_ev ev -> LogLevel -> exec_ev -> m ()

class (Monad m) => MonadLoggerConsoleAdd m where
  addLogger :: LoggerCtx e a -> OutputHandle LogOutputConsole -> m ()


class Monad m => MonadTime m where
  getNow :: m Time
  default getNow :: (MonadTrans t, MonadTime m', m ~ t m') => m Time
  getNow = lift getNow


class Monad m => MonadUser m where
  getUser :: m User


class Monad m => MonadRunCtxGet m where
  getRunCtx :: m e



class Monad m => MonadEventQueueListen m where
  listenEventQueue :: RunCtx exec_ev ev env opts cfg -> m (BroadcastChan Out (LogEventWithDetails (LogCmd exec_ev ev)))
  default listenEventQueue :: (MonadTrans t, MonadEventQueueListen m', m ~ t m') => RunCtx exec_ev ev env opts cfg -> m (BroadcastChan Out (LogEventWithDetails (LogCmd exec_ev ev)))
  listenEventQueue = lift . listenEventQueue


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

--------------------------------------------------------------------------------

instance MonadUnixSignals IO where
    addSignalHandler run_ctx sigs fn = do
        liftIO $ do
            mapM_ (\sig -> Posix.installHandler sig (runSigHandler fn run_ctx) Nothing) sigs

    watchSignal run_ctx sigs = do
        mapM_ (\sig -> Posix.installHandler sig (logSignal run_ctx) Nothing) sigs


logSignal :: RunCtx exec_ev ev env opts cfg -> Posix.Handler
logSignal run_ctx = Posix.CatchInfo \(Posix.SignalInfo sig _errno _si) -> do
    AMF.API.logAMFEvent (run_ctx ^. runCtxLogger) LogLevelVerbose (AMFEvSigReceived (UnixSignal sig))


runSigHandler :: (RunCtx exec_ev ev env opts cfg -> Posix.Signal -> IO ()) -> RunCtx exec_ev ev env opts cfg -> Posix.Handler
runSigHandler fn run_ctx = Posix.CatchInfo \(Posix.SignalInfo sig _errno _si) -> do
    AMF.API.logAMFEvent (run_ctx ^. runCtxLogger) LogLevelVerbose (AMFEvSigReceived (UnixSignal sig))
    fn run_ctx sig


instance MonadUnixSignalsRaise IO where
    raiseSignal run_ctx sig = do
        AMF.API.logAMFEvent (run_ctx ^. runCtxLogger) LogLevelVerbose (AMFEvSigSent (UnixSignal sig))
        liftIO (Posix.raiseSignal sig)


instance MonadConfigGet IO where
    getConfig run_ctx name = do
        cfg_map <- readTVarIO (run_ctx ^. runCtxConfig)
        case Map.lookup name cfg_map of
            Nothing -> pure Nothing
            Just v  -> do
                pure (Just v)

instance MonadConfigChangeBlockingReact IO where
    setConfigDefault _run_ctx _cfg = do
        pass

    setConfigBlockingReadAndParseFor _run_ctx _fn = do
        pass

instance MonadEventLogger IO where
    logEvent logger_ctx lvl ev = liftIO $ AMF.Logging.logEvent logger_ctx lvl ev
    logAMFEvent logger_ctx lvl ev = liftIO $ AMF.Logging.logAMFEvent logger_ctx lvl ev
    logExecEvent logger_ctx lvl ev = liftIO $ AMF.Logging.logExecEvent logger_ctx lvl ev

instance MonadLoggerConsoleAdd IO where
    addLogger log_ctx out = addConsoleLogger log_ctx out


instance MonadTime IO where
    getNow = now


instance MonadEventQueueRead IO where
    readEventQueue ch = liftIO (BroadcastChan.readBChan ch)

instance MonadEventQueueListen IO where
    listenEventQueue run_ctx = liftIO (newBChanListener (run_ctx ^. runCtxLogger . loggingCtxIntInEv))

instance Eventable a => MonadEventQueueWrite IO a where
    writeEventQueue ch msg = liftIO (BroadcastChan.writeBChan ch msg)

instance MonadEventQueueClose IO where
    closeEventQueue ch = liftIO (closeBChan ch)

--------------------------------------------------------------------------------
