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
import qualified Data.Aeson                    as Aeson
import           Codec.Serialise               as CBOR

-- local
import           AMF.Events
import           AMF.Executor.Common           as Common
import           AMF.Logging
import           AMF.Types.AppSpec
import           AMF.Types.Executor


data K8s = K8s
    { _appName :: Text
    }

data EventK8s = EventK8s
    deriving stock (Generic, Show)
    deriving anyclass (Serialise, Aeson.ToJSON)

instance Eventable EventK8s where
    toFmt fmt hn ln ts (pid, tid) lvl ev = case fmt of
        LogFormatLine -> Just (defaultLinePrefixFormatter hn ln ts (pid, tid) lvl <+> k8sEvLineFmt ev)
        LogFormatJSON -> Just (Aeson.encode ev <> "\n")
        LogFormatCBOR -> Just (CBOR.serialise ev)
        LogFormatCSV  -> Nothing
      where
        k8sEvLineFmt :: EventK8s -> LByteString
        k8sEvLineFmt k8s_ev = evFmt k8s_ev <> "\n"

        evFmt = \case
            EventK8s -> ""

instance FS K8s where
    fsDirRoot (K8s app_name) = do
        let x = parseAbsDir (toString ("/" <> app_name))
        case x of
            Nothing -> [absdir|/|]
            Just d  -> d

    fsDirMetadata _ = [reldir|etc|]
    fsDirLogs _ = [reldir|var/logs|]

    fsFileAppInfo _ = [relfile|appinfo|]

    fsDirCache _ = [reldir|var/cache|]


instance MonadIO m => Executor m EventK8s K8s where
    initExec _ _ = do
        oidcCache     <- atomically $ newTVar Map.empty
        (_mgr, _kcfg) <- liftIO (mkKubeClientConfig oidcCache KubeConfigCluster)

        pure (Right (K8s "TODO"))

    setupExec _ _ = pure (Left "TODO")

    finishExec _ _ = pure (Left "TODO")

runAppSpecAsK8s :: (Eventable ev, Envy.FromEnv env, Show ev, Show cfg) => AppSpec IO K8s EventK8s ev env opts cfg a -> IO ()
runAppSpecAsK8s app = do
    log_cfg <- newConfig newEmptyOutputs
    logger  <- newLoggingCtx log_cfg
    runAppSpec app logger


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
