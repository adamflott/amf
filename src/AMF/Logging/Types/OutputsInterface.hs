module AMF.Logging.Types.OutputsInterface
    ( Output(..)
    , HealthCheck(..)
    ) where

import           Relude

import           AMF.Logging.Types.Format

type TODOError = Text

data HealthCheck
    = Continue
    | Drop
    deriving stock Show

class MonadIO m => Output m a where
  data OutputHandle a :: *
  data OutputRetry a :: *
  data OutputRetryQueue a :: *

  outputFormat :: OutputHandle a -> m LogFormat

  openOutput :: a -> m (Either TODOError (OutputHandle a))
  closeOutput :: OutputHandle a -> m (Maybe TODOError)
  writeOutput :: OutputHandle a -> LByteString -> m ()

  healthCheck :: OutputHandle a -> m HealthCheck

--  default healthCheck :: OutputHandle a -> m HealthCheck
--  healthCheck _ = pure Continue


--instance Output IO (OutputHandle a)
