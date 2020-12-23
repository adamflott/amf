module AMF.Types.Executor where

import           Relude

-- Hackage
import           Path

-- local
import           AMF.API
import           AMF.Types.Environment
import           AMF.Types.FileSystem
import           AMF.Types.RunCtx


class Executor a where
  fsDirRoot :: a -> Maybe (Path Abs Dir)
  fsDirMetadata :: a -> Maybe (Path Rel Dir)
  fsDirLogs :: a -> Maybe (Path Rel Dir)
  fsFileAppInfo :: a -> Maybe (Path Rel File)
  fsDirCache :: a -> Maybe (Path Rel Dir)

  initExec :: (MonadIO m, MonadEnv m) => RunCtx ev opts cfg -> m (Either Text a)
  setupExec ::  (MonadIO m, MonadEnv m, MonadEventLogger m, MonadFileSystemRead m, Show cfg) => RunCtx ev opts cfg -> a -> m (Either Text a)
  finishExec :: (MonadIO m, MonadEventLogger m) => RunCtx ev opts cfg -> a -> m (Either Text a)


fsDirJoin :: Maybe (Path b Dir) -> [Maybe (Path Rel Dir)] -> Maybe (Path b Dir)
fsDirJoin (Just d) (Just d' : ds) = fsDirJoin (Just (d </> d')) ds
fsDirJoin (Just d) []             = Just d
fsDirJoin _        _              = Nothing

fsFileJoin :: Maybe (Path b Dir) -> Path Rel File -> Maybe (Path b File)
fsFileJoin (Just d) f = Just (d </> f)
fsFileJoin _        _ = Nothing
