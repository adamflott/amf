module AMF.Types.Executor where

import           Relude

-- Hackage
import           Path

-- local
import           AMF.API
import           AMF.Events
import           AMF.Logging.Types
import           AMF.Types.Environment
import           AMF.Types.FileSystem
import           AMF.Types.RunCtx



class FS a where
  fsDirRoot :: a -> Path Abs Dir
  fsDirMetadata :: a -> Path Rel Dir
  fsDirLogs :: a ->  Path Rel Dir
  fsFileAppInfo :: a -> Path Rel File
  fsDirCache :: a -> Path Rel Dir

class (Eventable exec_ev, FS a, MonadIO m) => Executor m exec_ev a where
  initExec :: (MonadEnv m, MonadEventLogger m) => RunCtx exec_ev ev env opts cfg -> LoggerCtx exec_ev ev -> m (Either Text a)
  setupExec ::  (MonadEnv m, MonadEventLogger m, MonadFileSystemRead m, Show cfg) => RunCtx exec_ev ev env opts cfg -> a -> m (Either Text a)
  finishExec :: (MonadEventLogger m) => RunCtx exec_ev ev env opts cfg -> a -> m (Either Text a)


fsDirJoin :: (Path b Dir) -> [(Path Rel Dir)] -> (Path b Dir)
fsDirJoin (d) (d' : ds) = fsDirJoin ((d </> d')) ds
fsDirJoin (d) []        = d
-- fsDirJoin _        _              = Nothing

fsFileJoin :: (Path b Dir) -> Path Rel File -> (Path b File)
fsFileJoin (d) f = (d </> f)
--fsFileJoin _        _ = Nothing


{-
fsDirJoin :: Maybe (Path b Dir) -> [Maybe (Path Rel Dir)] -> Maybe (Path b Dir)
fsDirJoin (Just d) (Just d' : ds) = fsDirJoin (Just (d </> d')) ds
fsDirJoin (Just d) []             = Just d
fsDirJoin _        _              = Nothing

fsFileJoin :: Maybe (Path b Dir) -> Path Rel File -> Maybe (Path b File)
fsFileJoin (Just d) f = Just (d </> f)
fsFileJoin _        _ = Nothing
-}
