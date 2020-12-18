module AMF.Logging.Types.OutputsInterface
    ( Output(..)
    , HealthCheck(..)
    ) where

import           Relude

-- local
import           AMF.Logging.Types.Format

data HealthCheck
    = Continue
    | Drop
    deriving stock Show

class MonadIO m => Output m a where
  type Error a

  data OutputHandle a :: *

  data OutputRetry a :: *
  data OutputRetryQueue a :: *

  outputFormat :: OutputHandle a -> m LogFormat

  openOutput :: a -> m (Either (Error a) (OutputHandle a))
  closeOutput :: OutputHandle a -> m (Maybe (Error a))
  writeOutput :: OutputHandle a -> LByteString -> m ()

  healthCheck :: OutputHandle a -> m HealthCheck
