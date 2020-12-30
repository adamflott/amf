module AMF.Logging.Types.Format
    ( LogFormat(..)
    ) where

import           Relude

-- base
import qualified Text.Show                      ( Show(..) )

-- Hackage
import           Data.YAML

data LogFormat
    = LogFormatLine
    | LogFormatJSON
    | LogFormatCSV
    | LogFormatCBOR
    deriving stock (Bounded, Enum, Eq, Generic)

instance Show LogFormat where
    show = \case
        LogFormatLine -> "line"
        LogFormatJSON -> "json"
        LogFormatCSV  -> "csv"
        LogFormatCBOR -> "cbor"


instance FromYAML LogFormat where
    parseYAML = withStr "log.format" $ \case
        "line" -> pure LogFormatLine
        "json" -> pure LogFormatJSON
        "csv"  -> pure LogFormatCSV
        "cbor" -> pure LogFormatCBOR
        other  -> fail (toString other <> " is not a supported log format. Supported formats: " <> availableFormats)
            where availableFormats = intercalate "," (fmap show [minBound :: LogFormat .. maxBound])
