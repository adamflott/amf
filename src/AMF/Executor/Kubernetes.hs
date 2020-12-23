module AMF.Executor.Kubernetes where

import           Relude

-- base
--import Data.Function          ((&))

-- Hackage
import           Path
import           Kubernetes.Client              ( KubeConfigSource(..)
                                                , mkKubeClientConfig
                                                )
--import Kubernetes.OpenAPI     (Accept (..), MimeJSON (..), dispatchMime)
--import Network.TLS            (credentialLoadX509)
import qualified Data.Map.Strict               as Map
import qualified System.Envy                   as Envy

-- local
import           AMF.Events
import           AMF.Executor.Common           as Common
import           AMF.Types.AppSpec
import           AMF.Types.Executor


data K8s = K8s
    { _appName :: Text
    }

instance Executor K8s where
    fsDirRoot (K8s app_name) = parseAbsDir (toString ("/" <> app_name))

    fsDirMetadata _ = Just [reldir|etc|]
    fsDirLogs _ = Just [reldir|var/logs|]

    fsFileAppInfo _ = Nothing

    fsDirCache _ = Just [reldir|var/cache|]

    initExec _ = do
        oidcCache     <- atomically $ newTVar Map.empty
        (_mgr, _kcfg) <- liftIO (mkKubeClientConfig oidcCache KubeConfigCluster)

        pure (Right (K8s "TODO"))

    setupExec _ _ = pure (Left "TODO")

    finishExec _ _ = pure (Left "TODO")

runAppSpecAsK8s :: (Eventable ev, Envy.FromEnv env, Show ev, Show cfg) => AppSpec IO K8s ev env opts cfg a -> IO ()
runAppSpecAsK8s = runAppSpec


{-
$ ls -al /var/run/secrets/
total 16
drwxr-xr-t 1 root root 4096 Dec 11 19:26 .
drwxr-xr-x 1 root root 4096 Oct 29 17:55 ..
drwxr-xr-x 3 root root 4096 Dec 11 19:26 kubernetes.io
$ ls -al /var/run/secrets/kubernetes.io/
total 8
drwxr-xr-x 3 root root 4096 Dec 11 19:26 .
drwxr-xr-t 1 root root 4096 Dec 11 19:26 ..
drwxrwsrwt 3 root 1337  140 Dec 11 19:26 serviceaccount
$ ls -al /var/run/secrets/kubernetes.io/serviceaccount/
total 4
drwxrwsrwt 3 root 1337  140 Dec 11 19:26 .
drwxr-xr-x 3 root root 4096 Dec 11 19:26 ..
drwxr-sr-x 2 root 1337  100 Dec 11 19:26 ..2020_12_11_19_26_28.551789689
lrwxrwxrwx 1 root root   31 Dec 11 19:26 ..data -> ..2020_12_11_19_26_28.551789689
lrwxrwxrwx 1 root root   13 Dec 11 19:26 ca.crt -> ..data/ca.crt
lrwxrwxrwx 1 root root   16 Dec 11 19:26 namespace -> ..data/namespace
lrwxrwxrwx 1 root root   12 Dec 11 19:26 token -> ..data/token
-}
