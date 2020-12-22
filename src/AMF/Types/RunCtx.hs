module AMF.Types.RunCtx where

import           Relude

-- Hackage
import           Control.Lens
import           Path
import           Chronos
import qualified Data.YAML                     as YAML

-- local
import           AMF.Logging.Types
import           AMF.Types.SystemInfo

data ConfigParseResult
    = ConfigParseResultIO Text
    | ConfigParseResultYAML (YAML.Pos, String)
    deriving stock Show

data ConfigParser a = ConfigParserYAML (LByteString -> Either ConfigParseResult a)

yamlParser :: YAML.FromYAML a => ConfigParser a
yamlParser =
    (ConfigParserYAML $ \v -> do
        let r = YAML.decode1 v
        case r of
            Left  err -> Left (ConfigParseResultYAML err)
            Right v'  -> Right v'
    )


data RunCtx ev cfg = RunCtx
    { _runCtxAppName       :: Text
    , _runCtxSystemInfo    :: SystemInfo
    , _runCtxEnvVars       :: [(Text, Text)]
    , _runCtxArgs          :: [Text]
    , _runCtxCwd           :: Path Abs Dir
    , _runCtxStartTime     :: Time
    , _runCtxLogger        :: LoggerCtx ev
    , _runCtxConfigParser  :: ConfigParser cfg
    , _runCtxConfigDefault :: Maybe cfg
    , _runCtxConfig        :: TVar (Map Text cfg)
    }

makeLenses ''RunCtx
