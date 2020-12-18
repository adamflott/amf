module AMF.API where

import           Relude

-- Hackage
import           BroadcastChan
import           Chronos
import           Control.Lens
import           Path
import qualified System.Posix                  as Posix

-- local
import           AMF.Events
import           AMF.Logging.Types
import           AMF.Types.Executor
import           AMF.Types.SystemInfo


data ConfigEvent
    = ConfigEventModified
    | ConfigEventDeleted
    | ConfigEventAdded

makeClassy ''ConfigEvent

newtype LogEvent a = LogEvent a


class Monad m => MonadUnixSignals m where
  addSignalHandler :: RunCtx -> [Posix.Signal] -> (RunCtx -> Posix.Handler) -> m ()
  default addSignalHandler :: (MonadTrans t, MonadUnixSignals m', m ~ t m') => RunCtx -> [Posix.Signal] -> (RunCtx -> Posix.Handler) -> m ()
  addSignalHandler run_ctx sigs fn = lift (addSignalHandler run_ctx sigs fn)

class Monad m => MonadReadConfig m where
  getXConfig :: m a

class Monad m => MonadReactConfig m where
  addReactConfigHandler :: Path b File -> (ConfigEvent -> m ()) -> m ()

class Monad m => MonadEventLogger m where
  logEvent :: Eventable a => LoggerCtx a -> LogEvent a -> m ()

class Monad m => MonadTime m where
  getNow :: m Time
  default getNow :: (MonadTrans t, MonadTime m', m ~ t m') => m Time
  getNow = lift getNow

class Monad m => MonadUser m where
  getUser :: m User

class Monad m => MonadRunCtxGet m where
  getRunCtx :: m RunCtx

class Monad m => MonadEventQueueRead m where
  readEventQueue :: BroadcastChan Out AMFEvent -> m (Maybe AMFEvent)

  default readEventQueue :: (MonadTrans t, MonadEventQueueRead m', m ~ t m') => BroadcastChan Out AMFEvent -> m (Maybe AMFEvent)
  readEventQueue = lift . readEventQueue

class Monad m => MonadEventQueueGetOut m where
  getAMFEventQueueOut :: m (BroadcastChan Out AMFEvent)

class (Monad m, Eventable e) => MonadEventQueueWrite m e where
  writeEventQueue :: BroadcastChan In AMFEvent -> e -> m Bool

class Monad m => MonadEventQueueClose m where
  closeEventQueue :: BroadcastChan In AMFEvent -> m Bool


--------------------------------------------------------------------------------

instance MonadTime IO where
    getNow = now

instance MonadUnixSignals IO where
    addSignalHandler run_ctx sigs fn = do
        liftIO $ do
            mapM_ (\sig -> Posix.installHandler sig (fn run_ctx) Nothing) sigs

instance MonadEventQueueRead IO where
    readEventQueue ch = liftIO (BroadcastChan.readBChan ch)

instance MonadEventQueueWrite IO AMFEvent where
    writeEventQueue ch msg = liftIO (BroadcastChan.writeBChan ch msg)

instance MonadEventQueueClose IO where
    closeEventQueue ch = liftIO (closeBChan ch)
