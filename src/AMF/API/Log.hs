module AMF.API.Log where

import           Relude

-- base
import           GHC.ByteOrder                  ( targetByteOrder )

-- Hackage
import           Control.Lens
import           Path
import qualified System.Statgrab               as SG

-- local
import           AMF.API
import           AMF.Events
import           AMF.Logging.Types.Level
import           AMF.Types.Executor
import           AMF.Types.RunCtx
import           AMF.Types.SystemInfo


logAllSysInfo :: (MonadEventLogger m, Executor x) => RunCtx e c -> x -> m ()
logAllSysInfo run_ctx e = do
    logSysInfo run_ctx e
    logSysLimitInfo run_ctx e
    logSysCompilerInfo run_ctx e
    logExecutorFsEntries run_ctx e

logSysInfo :: MonadEventLogger m => RunCtx e c -> p -> m ()
logSysInfo run_ctx _ = do
    let si        = run_ctx ^. runCtxSystemInfo
        sg        = si ^. systemInfoStats
        shi       = sg ^. statGrabInfoHostInfo
        ncpu      = fromIntegral (SG.hostNCPU shi)

        mem       = sg ^. statGrabInfoHostMemory
        mem_used  = fromIntegral (SG.memUsed mem)
        mem_total = fromIntegral (SG.memTotal mem)

        bw        = SG.hostBitWidth shi
        bo        = GHC.ByteOrder.targetByteOrder
        os_name   = decodeUtf8 (SG.hostOsName shi)
        os_rel    = decodeUtf8 (SG.hostOsRelease shi)
        os_vers   = decodeUtf8 (SG.hostOsVersion shi)
        plat      = decodeUtf8 (SG.hostPlatform shi)

    AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvSysInfo ncpu (mem_used, mem_total) bw bo os_name os_rel os_vers plat)

logSysLimitInfo :: MonadEventLogger m => RunCtx e c -> p -> m ()
logSysLimitInfo run_ctx _ = do
    let rls = run_ctx ^. runCtxSystemInfo . systemInfoResourceLimits
    AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvSysLimitInfo rls)

logSysCompilerInfo :: MonadEventLogger m => RunCtx e c -> p -> m ()
logSysCompilerInfo run_ctx _ = do
    let name = toText (run_ctx ^. runCtxSystemInfo . systemInfoCompilerName)
        vers = run_ctx ^. runCtxSystemInfo . systemInfoCompilerVersion
    AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvSysCompilerInfo name vers)

logExecutorFsEntries :: (MonadEventLogger m, Executor a) => RunCtx e c -> a -> m ()
logExecutorFsEntries run_ctx e = do
    AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvFSEntry "root" (f (fsDirRoot e)))
    AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvFSEntry "metadata" (f (fsDirMetadata e)))
    AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvFSEntry "logs" (f (fsDirLogs e)))
    AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvFSEntry "app_info" (f (fsFileAppInfo e)))
    AMF.API.logAMFEvent run_ctx LogLevelVerbose (AMFEvFSEntry "cache" (f (fsDirCache e)))
  where
    f :: Maybe (Path b t) -> Text
    f = maybe "N/A" (toText . toFilePath)
