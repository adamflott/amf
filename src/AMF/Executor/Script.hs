module AMF.Executor.Script where

import           Relude

-- base

-- Hackage
import qualified System.Envy                   as Envy

-- local
import           AMF.Events
import           AMF.Executor.Common           as Common
import           AMF.Types.AppSpec
import           AMF.Types.Environment
import           AMF.Types.Executor


data Script = Script
    { _appName :: Text
    }
    deriving stock Generic

instance Executor Script where
    fsDirRoot _ = Nothing
    fsDirMetadata _ = Nothing
    fsDirLogs _ = Nothing
    fsFileAppInfo _ = Nothing
    fsDirCache _ = Nothing

    initExec _run_ctx = do
        pn <- getProgName
        pure (Right (Script pn))

    setupExec _ ctx = pure (Right ctx)

    finishExec _run_ctx ctx = do
        pure (Right ctx)

runAppSpecAsScript :: (Eventable ev, Envy.FromEnv env, Show ev, Show cfg) => AppSpec IO Script ev env opts cfg a -> IO ()
runAppSpecAsScript = runAppSpec
