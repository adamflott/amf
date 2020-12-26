module AMF.Types.Config where

import           Relude

-- base
import           Text.Show

-- Hackage
import           Validation


data ConfigParseErr
    = ConfigParseErrIO Text
    | forall a. Show a => ConfigParserErr a

instance Show ConfigParseErr where
    show = \case
        ConfigParseErrIO e -> Relude.show e
        ConfigParserErr  e -> Relude.show e

data ConfigParser a = ConfigParser Text (LByteString -> Either ConfigParseErr a)
newtype ConfigValidator a = ConfigValidator (a -> Validation ConfigParseErr a)

data ConfigSpec a = ConfigSpec
    { _confSpecParser    :: ConfigParser a
    , _confSpecValidator :: ConfigValidator a
    }
