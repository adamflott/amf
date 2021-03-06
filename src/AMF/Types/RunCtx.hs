module AMF.Types.RunCtx where

import           Relude

-- base

-- Hackage
import           Control.Lens
import           Path
import           Chronos

-- local
import           AMF.Logging.Types
import           AMF.Types.SystemInfo
import           AMF.Types.Config


data RunCtx exec_ev ev env opts cfg = RunCtx
    { _runCtxAppName         :: Text
    , _runCtxSystemInfo      :: SystemInfo
    , _runCtxEnvVars         :: [(Text, Text)]
    , _runCtxArgs            :: [Text]
    , _runCtxCwd             :: Path Abs Dir
    , _runCtxStartTime       :: Time
    , _runCtxLogger          :: LoggerCtx exec_ev ev
    , _runCtxEnv             :: env
    , _runCtxOptions         :: opts
    , _runCtxConfigParser    :: ConfigParser cfg
    , _runCtxConfigValidator :: ConfigValidator cfg
    , _runCtxConfigDefault   :: Maybe cfg
    , _runCtxConfig          :: TVar (Map Text cfg)
    }

makeLenses ''RunCtx
