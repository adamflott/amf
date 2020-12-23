module AMF.Types.Config where

import           Relude

-- base
import           Text.Show

-- Hackage


data ConfigParseErr
    = ConfigParseErrIO Text
    | forall a. Show a => ConfigParserErr a

instance Show ConfigParseErr where
    show = \case
        ConfigParseErrIO e -> Relude.show e
        ConfigParserErr  e -> Relude.show e

data ConfigParser b = ConfigParser Text (LByteString -> Either ConfigParseErr b)

data ConfigSpec b = ConfigSpec
    { _confSpecParser :: ConfigParser b
    }
