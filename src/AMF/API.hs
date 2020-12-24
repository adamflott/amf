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
import           AMF.Types.Common
import           AMF.Events
import           AMF.Logging
import           AMF.Logging.Types
import           AMF.Logging.Types.Console
import           AMF.Logging.Types.Level
import           AMF.Logging.Types.OutputsInterface
import           AMF.Types.SystemInfo
import           AMF.Types.RunCtx



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
  addSignalHandler :: RunCtx ev env opts cfg -> [Posix.Signal] -> (RunCtx ev env opts cfg -> Posix.Signal -> IO ()) -> m ()
  default addSignalHandler :: (MonadTrans t, MonadUnixSignals m', m ~ t m') => RunCtx ev env opts cfg -> [Posix.Signal] -> (RunCtx ev env opts cfg -> Posix.Signal -> IO ()) -> m ()
  addSignalHandler run_ctx sigs fn = lift (addSignalHandler run_ctx sigs fn)

class (Monad m, MonadEventLogger m) => MonadUnixSignalsRaise m where
  raiseSignal :: RunCtx ev env opts cfg -> Posix.Signal -> m ()
  default raiseSignal :: (MonadTrans t, MonadUnixSignalsRaise m', m ~ t m') => RunCtx ev env opts cfg -> Posix.Signal -> m ()
  raiseSignal run_ctx sig = lift (raiseSignal run_ctx sig)


class Monad m => MonadConfigGet m where
  getConfig :: RunCtx ev env opts cfg -> Text -> m (Maybe cfg)
  default getConfig :: (MonadTrans t, MonadConfigGet m', m ~ t m') => RunCtx ev env opts cfg -> Text -> m (Maybe cfg)
  getConfig run_ctx name = lift (getConfig run_ctx name)

class Monad m => MonadConfigChangeBlockingReact m where
  setConfigDefault :: RunCtx ev env opts cfg -> Maybe c -> m ()
  setConfigBlockingReadAndParseFor :: RunCtx ev env opts cfg -> Text -> m ()

  --addReactConfigHandler :: RunCtx e c -> Text -> (RunCtx e c -> FSEvent -> IO ()) -> m ()
  --default addReactConfigHandler :: (MonadTrans t, MonadConfigChangeReact m', m ~ t m') => RunCtx e c -> Text -> (RunCtx e c -> FSEvent -> IO ()) -> m ()
  --addReactConfigHandler run_ctx fn f = lift (addReactConfigHandler run_ctx fn f)


class Monad m => MonadEventLogger m where
  logEvent :: RunCtx ev env opts cfg -> LogLevel -> ev -> m ()
  logAMFEvent :: RunCtx ev env opts cfg -> LogLevel -> AMFEvent -> m ()

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



class Monad m => MonadEventQueueListen m where
  listenEventQueue :: RunCtx ev env opts cfg -> m (BroadcastChan Out (LogEventWithDetails (LogCmd ev)))
  default listenEventQueue :: (MonadTrans t, MonadEventQueueListen m', m ~ t m') => RunCtx ev env opts cfg -> m (BroadcastChan Out (LogEventWithDetails (LogCmd ev)))
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

runSigHandler :: (RunCtx ev env opts cfg -> Posix.Signal -> IO ()) -> RunCtx ev env opts cfg -> Posix.Handler
runSigHandler fn run_ctx = Posix.CatchInfo \(Posix.SignalInfo sig _errno _si) -> do
    AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvSigReceived (UnixSignal sig))
    fn run_ctx sig


instance MonadUnixSignalsRaise IO where
    raiseSignal run_ctx sig = do
        AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvSigSent (UnixSignal sig))
        liftIO (Posix.raiseSignal sig)


instance MonadConfigGet IO where
    getConfig run_ctx name = do
        cfg_map <- readTVarIO (run_ctx ^. runCtxConfig)
        case Map.lookup name cfg_map of
            Nothing -> pure Nothing
            Just v  -> do
                pure (Just v)

instance MonadConfigChangeBlockingReact IO where
    setConfigDefault run_ctx cfg = do
        pass

    setConfigBlockingReadAndParseFor run_ctx fn = do
        pass

instance MonadEventLogger IO where
    logEvent run_ctx lvl ev = liftIO $ AMF.Logging.logEvent (run_ctx ^. runCtxLogger) lvl ev

    logAMFEvent run_ctx lvl ev = liftIO $ AMF.Logging.logAMFEvent (run_ctx ^. runCtxLogger) lvl ev

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
