module AMF.Types.Environment where

-- prelude
import           Relude

-- base
import qualified System.Environment            as IO

-- Hackage
import           Control.Exception.Safe         ( tryIO )


class Monad m => MonadEnv m where
  isEnvVar :: Text -> m Bool
  getEnvVar :: Text -> Text -> m Text
  getEnvironment :: m [(Text, Text)]

  default isEnvVar :: (MonadTrans t, MonadEnv m', m ~ t m') => Text -> m Bool
  isEnvVar var = lift $ isEnvVar var

  default getEnvVar :: (MonadTrans t, MonadEnv m', m ~ t m') => Text -> Text -> m Text
  getEnvVar var def = lift $ getEnvVar var def

  default getEnvironment :: (MonadTrans t, MonadEnv m', m ~ t m') => m [(Text, Text)]
  getEnvironment = lift getEnvironment


  setEnvVar :: Text -> Text -> m ()
  unsetEnvVar :: Text -> m ()

  default setEnvVar :: (MonadTrans t, MonadEnv m', m ~ t m') => Text -> Text -> m ()
  setEnvVar name val = lift $ setEnvVar name val

  default unsetEnvVar :: (MonadTrans t, MonadEnv m', m ~ t m') => Text -> m ()
  unsetEnvVar var = lift $ unsetEnvVar var



instance MonadEnv IO where
    isEnvVar var = do
        maybe_val <- IO.lookupEnv (toString var)
        case maybe_val of
            Nothing -> pure False
            Just _  -> pure True

    getEnvVar var def = do
        maybe_val <- IO.lookupEnv (toString var)
        case maybe_val of
            Nothing  -> pure def
            Just val -> pure (toText val)

    getEnvironment = fmap (\(a1, a2) -> (toText a1, toText a2)) <$> IO.getEnvironment

    setEnvVar name val = do
        void $ tryIO $ IO.setEnv (toString name) (toString val)

    unsetEnvVar var = do
        void $ tryIO $ IO.unsetEnv (toString var)


instance MonadEnv m => MonadEnv (ReaderT r m)
instance MonadEnv m => MonadEnv (StateT r m)
instance MonadEnv m => MonadEnv (ExceptT r m)


class Monad m => MonadArguments m where
  getArgs :: m [Text]

  default getArgs :: (MonadTrans t, MonadArguments m', m ~ t m') => m [Text]
  getArgs = lift getArgs

instance MonadArguments IO where
    getArgs = fmap toText <$> IO.getArgs

instance MonadArguments m => MonadArguments (ReaderT r m)
instance MonadArguments m => MonadArguments (StateT r m)
instance MonadArguments m => MonadArguments (ExceptT r m)
