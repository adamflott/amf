module AMF.Logging.Types.Console
    ( StdOutOrErr(..)
    , LogOutputConsole(..)
    ) where

-- prelude
import           Relude

-- base
import           System.IO                      ( BufferMode(..)
                                                , hClose
                                                , hIsClosed
                                                , hSetBuffering
                                                )
import qualified Text.Show                      ( Show(..) )

-- Hackage
import qualified System.IO.Utf8                as Utf8
import           Control.Monad.Catch            ( MonadMask )
import qualified Data.ByteString.Lazy          as BSL
import           Data.YAML

-- local
import           AMF.Logging.Types.Format
import           AMF.Logging.Types.Level
import           AMF.Logging.Types.OutputsInterface


data StdOutOrErr
    = LogOutputStdOut
    | LogOutputStdErr
    deriving stock (Bounded, Enum, Eq, Generic)

instance Show StdOutOrErr where
    show = \case
        LogOutputStdOut -> "stdout"
        LogOutputStdErr -> "stderr"

instance FromYAML StdOutOrErr where
    parseYAML = withStr "log.output.console.stdout_or_stderr" $ \case
        "stdout" -> pure LogOutputStdOut
        "stderr" -> pure LogOutputStdErr
        other    -> fail (toString other <> " is not a supported console output format. Supported formats: " <> availableFormats)
            where availableFormats = intercalate "," (fmap show [minBound :: StdOutOrErr .. maxBound])

data LogOutputConsole = LogOutputConsole LogLevel LogFormat StdOutOrErr Handle
    deriving stock Show


instance (MonadIO m, MonadMask m) => Output m LogOutputConsole where
    type Error LogOutputConsole = Text
    
    data OutputHandle LogOutputConsole = CHandle Handle LogFormat

    outputFormat (CHandle _ fmt) = pure fmt

    openOutput (LogOutputConsole _ fmt stdout_or_err handle) = do
        closed <- liftIO (hIsClosed handle)
        if closed
            then pure (Left "handle is closed")
            else do
                let fh = case stdout_or_err of
                        LogOutputStdErr -> Relude.stderr
                        LogOutputStdOut -> Relude.stdout

                case fmt of
                    LogFormatLine -> do
                        liftIO (hSetBuffering fh LineBuffering)
                    LogFormatCSV -> do
                        liftIO (hSetBuffering fh LineBuffering)
                    LogFormatJSON -> do
                        liftIO (hSetBuffering fh NoBuffering)
                    LogFormatCBOR -> do
                        liftIO (hSetBuffering fh NoBuffering)

                pure (Right (CHandle handle fmt))

    closeOutput (CHandle handle _) = do
        liftIO (hClose handle)
        pure Nothing

    writeOutput (CHandle handle _) msg = do
        Utf8.withHandle handle (liftIO (BSL.hPut handle msg))

    healthCheck (CHandle handle _) = do
        closed <- liftIO (hIsClosed handle)
        if closed then pure Drop else pure Continue
