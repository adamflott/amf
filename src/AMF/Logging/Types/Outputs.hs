module AMF.Logging.Types.Outputs
    ( LogOutputs(..)

     -- * Lenses
    , logOutputsConsoles
    , logOutputsFiles
    ) where

-- prelude
import           Relude

-- Hackage
import           Control.Lens

-- local
import           AMF.Logging.Types.Console
import           AMF.Logging.Types.File


data LogOutputs = LogOutputs
    { _logOutputsConsoles :: [LogOutputConsole]
    , _logOutputsFiles    :: [LogOutputFile]
    }
    deriving stock Show

makeLenses ''LogOutputs
