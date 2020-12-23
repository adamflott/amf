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


data RunCtx ev opts cfg = RunCtx
    { _runCtxAppName       :: Text
    , _runCtxSystemInfo    :: SystemInfo
    , _runCtxEnvVars       :: [(Text, Text)]
    , _runCtxArgs          :: [Text]
    , _runCtxCwd           :: Path Abs Dir
    , _runCtxStartTime     :: Time
    , _runCtxLogger        :: LoggerCtx ev
    , _runCtxOptions       :: opts
    , _runCtxConfigParser  :: ConfigParser cfg
    , _runCtxConfigDefault :: Maybe cfg
    , _runCtxConfig        :: TVar (Map Text cfg)
    }

makeLenses ''RunCtx
