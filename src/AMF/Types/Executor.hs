module AMF.Types.Executor where

import           Relude

-- Hackage
import           Path
import           BroadcastChan
import           Chronos
import           Control.Lens

-- local
import           AMF.Types.Environment
import           AMF.Types.SystemInfo
import           AMF.Events


data RunCtx = RunCtx
    { _runCtxSystemInfo :: SystemInfo
    , _runCtxEnvVars    :: [(Text, Text)]
    , _runCtxArgs       :: [Text]
    , _runCtxCwd        :: Path Abs Dir
    , _runCtxStartTime  :: Time
    , _runCtxEventIn    :: BroadcastChan In AMFEvent
    , _runCtxEventOut   :: BroadcastChan Out AMFEvent
    }

makeLenses ''RunCtx


class Executor a where
  fsDirRoot :: a -> Maybe (Path Abs Dir)
  fsDirMetadata :: a -> Maybe (Path Rel Dir)
  fsDirLogs :: a -> Maybe (Path Rel Dir)
  fsFileAppInfo :: a -> Maybe (Path Rel File)
  fsDirCache :: a -> Maybe (Path Rel Dir)

  initExec :: MonadEnv m => RunCtx -> m (Either Text a)
  finishExec :: Monad m => RunCtx -> a -> m (Either Text a)
