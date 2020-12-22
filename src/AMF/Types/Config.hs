module AMF.Types.Config where

import           Relude

import qualified Data.YAML                     as YAML


data ConfigParseResult
    = ConfigParseResultIO Text
    | ConfigParseResultYAML (YAML.Pos, String)
    deriving stock Show


newtype ConfigParser a = ConfigParserYAML (LByteString -> Either ConfigParseResult a)

data ConfigSpec a = ConfigSpec
    { _confSpecParser :: ConfigParser a
    }
