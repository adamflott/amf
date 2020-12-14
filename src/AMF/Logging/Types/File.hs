module AMF.Logging.Types.File
    ( LogOutputFileWriteMode(..)
    , LogOutputFile(..)
    ) where

-- prelude
import           Relude

-- base
import           System.IO                      ( hClose
                                                , hIsClosed
                                                , hFlush
                                                )

-- Hackage
import           Path
import           Control.Monad.Catch            ( MonadMask )
import qualified Data.ByteString.Lazy          as BSL
import qualified System.IO.Utf8                as Utf8

-- local
import           AMF.Logging.Types.Format
import           AMF.Logging.Types.Level
import           AMF.Logging.Types.OutputsInterface
import           AMF.Types.FileSystem

data LogOutputFileWriteMode
    = Append
    | Overwrite
    deriving stock (Eq, Generic, Show)

data LogOutputFile = LogOutputFile LogLevel LogFormat (Path Abs File) LogOutputFileWriteMode
    deriving stock Show


toIOMode :: LogOutputFileWriteMode -> IOMode
toIOMode = \case
    Append    -> AppendMode
    Overwrite -> WriteMode

instance (MonadIO m, MonadMask m, MonadFileSystemRead m, MonadFileSystemWrite m) => Output m LogOutputFile where
    data OutputHandle LogOutputFile = FHandle Handle LogFormat

    outputFormat (FHandle _ fmt) = pure fmt

    openOutput (LogOutputFile _ fmt fp mode) = do
        maybe_fh <- openFile fp (toIOMode mode)
        case maybe_fh of
            Left  err -> pure (Left (show err))
            Right fh  -> do
                pure (Right (FHandle fh fmt))

    closeOutput (FHandle handle _) = do
        liftIO (hClose handle)
        pure Nothing

    writeOutput (FHandle handle _) msg = do
        Utf8.withHandle handle $ do
            liftIO (BSL.hPut handle msg)
            liftIO (hFlush handle)

    healthCheck (FHandle handle _) = do
        closed <- liftIO (hIsClosed handle)
        if closed then pure Drop else pure Continue
