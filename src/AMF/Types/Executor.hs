module AMF.Types.Executor where

import           Relude

-- Hackage
import           Path
import           Chronos
import           Control.Lens

-- local
import           AMF.Logging.Types
import           AMF.Types.Environment
import           AMF.Types.SystemInfo


data RunCtx a = RunCtx
    { _runCtxSystemInfo :: SystemInfo
    , _runCtxEnvVars    :: [(Text, Text)]
    , _runCtxArgs       :: [Text]
    , _runCtxCwd        :: Path Abs Dir
    , _runCtxStartTime  :: Time
    , _runCtxLogger     :: LoggerCtx a
    }

makeLenses ''RunCtx


class Executor a where
  fsDirRoot :: a -> Maybe (Path Abs Dir)
  fsDirMetadata :: a -> Maybe (Path Rel Dir)
  fsDirLogs :: a -> Maybe (Path Rel Dir)
  fsFileAppInfo :: a -> Maybe (Path Rel File)
  fsDirCache :: a -> Maybe (Path Rel Dir)

  initExec :: (MonadIO m, MonadEnv m) => RunCtx e -> m (Either Text a)
  finishExec :: Monad m => RunCtx e -> a -> m (Either Text a)
