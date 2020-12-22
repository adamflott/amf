module AMF.Executor.Common where

-- prelude
import           Relude

-- base
import           Data.Version                   ( Version )

-- Hackage

-- local
import           AMF.API
import           AMF.API.SystemInfo
import           AMF.Logging
import           AMF.Types.Config
import           AMF.Types.Environment
import           AMF.Types.FileSystem
import           AMF.Types.RunCtx

-- autogenerated
import           Paths_amf


amfVersion :: Version
amfVersion = Paths_amf.version

init :: (MonadIO m, MonadEnv m, MonadArguments m, MonadFileSystemRead m, MonadTime m) => Text -> ConfigParser cfg -> m (RunCtx ev cfg)
init app_name cfg_parser = do
    log_cfg <- newConfig newEmptyOutputs
    logger  <- newLoggingCtx log_cfg

    RunCtx
        <$> pure app_name
        <*> newSystemInfo
        <*> getEnvironment
        <*> getArgs
        <*> getCurrentDirectory
        <*> getNow
        <*> pure logger
        <*> pure cfg_parser
        <*> pure Nothing
        <*> newTVarIO mempty
