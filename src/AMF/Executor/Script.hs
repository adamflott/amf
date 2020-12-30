module AMF.Executor.Script where

import           Relude

-- base

-- Hackage
import qualified System.Envy                   as Envy
import qualified Data.Aeson                    as Aeson
import           Codec.Serialise               as CBOR
import           Path

-- local
import           AMF.Events
import           AMF.Executor.Common           as Common
import           AMF.Logging
import           AMF.Types.AppSpec
import           AMF.Types.Environment
import           AMF.Types.Executor


data Script = Script
    { _appName :: Text
    }
    deriving stock Generic

data EventScript = EventScript
    deriving stock (Generic, Show)
    deriving anyclass (Serialise, Aeson.ToJSON)

instance Eventable EventScript where
    toFmt fmt hn ln ts (pid, tid) lvl ev = case fmt of
        LogFormatLine -> Just (defaultLinePrefixFormatter hn ln ts (pid, tid) lvl <+> daemonEvLineFmt ev)
        LogFormatJSON -> Just (Aeson.encode ev <> "\n")
        LogFormatCBOR -> Just (CBOR.serialise ev)
        LogFormatCSV  -> Nothing

daemonEvLineFmt :: EventScript -> LByteString
daemonEvLineFmt ev = evFmt ev <> "\n"
  where
    evFmt = \case
        EventScript -> ""

instance FS Script where
    fsDirRoot _ = [absdir|/|]
    fsDirMetadata _ = [reldir|etc/|]
    fsDirLogs _ = [reldir|var/log|]
    fsFileAppInfo _ = [relfile|appinfo|]
    fsDirCache _ = [reldir|tmp|]

instance MonadIO m => Executor m EventScript Script where
    initExec _run_ctx _ = do
        pn <- getProgName
        pure (Right (Script pn))

    setupExec _ ctx = pure (Right ctx)

    finishExec _run_ctx ctx = do
        pure (Right ctx)

runAppSpecAsScript :: (Eventable ev, Envy.FromEnv env, Show ev, Show cfg) => AppSpec IO Script EventScript ev env opts cfg a -> IO ()
runAppSpecAsScript app = do
    log_cfg <- newConfig newEmptyOutputs
    logger  <- newLoggingCtx log_cfg
    runAppSpec app logger
