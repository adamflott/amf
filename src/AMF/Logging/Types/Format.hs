module AMF.Logging.Types.Format
    ( LogFormat(..)
    ) where

import           Relude

data LogFormat
    = LogFormatLine
    | LogFormatJSON
    | LogFormatCSV
    | LogFormatCBOR
    deriving stock (Eq, Generic, Show)
