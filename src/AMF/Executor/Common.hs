module AMF.Executor.Common where

-- prelude
import           Relude

-- base

-- Hackage
import           BroadcastChan

-- local
import           AMF.API
import           AMF.API.SystemInfo
import           AMF.Types.Environment
import           AMF.Types.FileSystem
import           AMF.Types.Executor


init :: (MonadIO m, MonadEnv m, MonadArguments m, MonadFileSystemRead m, MonadTime m) => m RunCtx
init = do
    ch_in  <- newBroadcastChan
    ch_out <- newBChanListener ch_in
    RunCtx <$> newSystemInfo <*> getEnvironment <*> getArgs <*> getCurrentDirectory <*> getNow <*> pure ch_in <*> pure ch_out
