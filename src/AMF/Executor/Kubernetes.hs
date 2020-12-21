module AMF.Executor.Kubernetes where

import Relude

--import Data.Function          ((&))
import Kubernetes.Client      (KubeConfigSource (..), mkKubeClientConfig)
--import Kubernetes.OpenAPI     (Accept (..), MimeJSON (..), dispatchMime)
--import Network.TLS            (credentialLoadX509)
import qualified Data.Map.Strict as Map

import           AMF.Types.Executor
--import           AMF.Executor.Common           as Common
--import           AMF.API
--import           AMF.Types.Environment
--import           AMF.Events

data K8s = K8s

instance Executor K8s where

  initExec _ = do
    oidcCache <- atomically $ newTVar Map.empty
    (_mgr, _kcfg) <- liftIO (mkKubeClientConfig oidcCache KubeConfigCluster)

    pure (Right K8s)

{-
redis@dpm-mpulse-raas-pod-0:/$ ls -al /var/run/secrets/
total 16
drwxr-xr-t 1 root root 4096 Dec 11 19:26 .
drwxr-xr-x 1 root root 4096 Oct 29 17:55 ..
drwxr-xr-x 3 root root 4096 Dec 11 19:26 kubernetes.io
redis@dpm-mpulse-raas-pod-0:/$ ls -al /var/run/secrets/kubernetes.io/
total 8
drwxr-xr-x 3 root root 4096 Dec 11 19:26 .
drwxr-xr-t 1 root root 4096 Dec 11 19:26 ..
drwxrwsrwt 3 root 1337  140 Dec 11 19:26 serviceaccount
redis@dpm-mpulse-raas-pod-0:/$ ls -al /var/run/secrets/kubernetes.io/serviceaccount/
total 4
drwxrwsrwt 3 root 1337  140 Dec 11 19:26 .
drwxr-xr-x 3 root root 4096 Dec 11 19:26 ..
drwxr-sr-x 2 root 1337  100 Dec 11 19:26 ..2020_12_11_19_26_28.551789689
lrwxrwxrwx 1 root root   31 Dec 11 19:26 ..data -> ..2020_12_11_19_26_28.551789689
lrwxrwxrwx 1 root root   13 Dec 11 19:26 ca.crt -> ..data/ca.crt
lrwxrwxrwx 1 root root   16 Dec 11 19:26 namespace -> ..data/namespace
lrwxrwxrwx 1 root root   12 Dec 11 19:26 token -> ..data/token
-}