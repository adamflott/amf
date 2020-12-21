module Main where

-- prelude
import           Relude

-- base
import           Control.Concurrent             ( threadDelay )
import           System.Exit

-- Hackage
import           Codec.Serialise               as CBOR
import           Control.Concurrent.Async
import           Control.Lens
import           Control.Monad.Catch            ( MonadMask )
import           Options.Applicative
import qualified Data.Aeson                    as Aeson
import qualified System.Posix                  as Posix

-- local
import           AMF.API
import           AMF.Events
import           AMF.Executor.Daemon
import           AMF.Types.Executor
import           AMF.Logging.Types
import           AMF.Logging.Outputs.Console
import           AMF.Logging.Types.Format
import           AMF.Logging.Types.Level
import           AMF.Logging.Types.Console
import           AMF.Types.Common

data DaemonType
    = DaemonTypeTraditional
    | DaemonTypeKubernetes
    deriving stock (Show)

data Options = Options
    { dt :: DaemonType
    }
    deriving stock Show



parseOpts :: IO Options
parseOpts = customExecParser (prefs (showHelpOnEmpty <> showHelpOnError)) appOpts


appOpts :: ParserInfo Options
appOpts = info (optSpec <**> helper) (fullDesc <> progDesc "example daemon")

optSpec :: Parser Options
optSpec = Options <$> (dtTrad <|> dtK8s)
  where
    dtTrad = flag' DaemonTypeTraditional (long "traditional" <> short 't' <> help "")
    dtK8s  = flag' DaemonTypeKubernetes (long "kubernetes" <> short 'k' <> help "")


--------------------------------------------------------------------------------

sigHandler :: RunCtx EventX -> Posix.Signal -> IO ()
sigHandler run_ctx _sig = do
    logEvent run_ctx LogLevelTerse (EventB 1)
    pass

--------------------------------------------------------------------------------

data EventX
    = EventA Text Text
    | EventB Int
    deriving stock (Generic, Show)
    deriving anyclass (Serialise, Aeson.ToJSON)



instance Eventable EventX where
    toFmt fmt hn ln ts (pid, tid) lvl ev = case fmt of
        LogFormatLine -> Just (defaultLinePrefixFormatter hn ln ts (pid, tid) lvl <+> daemonEvLineFmt ev)
        LogFormatJSON -> Just (Aeson.encode ev <> "\n")
        LogFormatCBOR -> Just (CBOR.serialise ev)
        LogFormatCSV  -> Nothing

daemonEvLineFmt :: EventX -> LByteString
daemonEvLineFmt ev = evFmt ev <> "\n"
  where
    evFmt = \case
        EventA t1 t2 -> "t1:" <> show t1 <+> "t2:" <> show t2
        EventB i1    -> "i1:" <> show i1

--------------------------------------------------------------------------------

type AppConstraints m
    = ( MonadIO m
      , MonadMask m
      , MonadFail m
      , MonadTime m
      , MonadEventLogger m
      , MonadLoggerConsoleAdd m
      , MonadUnixSignals m
      , MonadUnixSignalsRaise m
      , MonadEventQueueRead m
      , MonadEventQueueListen m
      )


myAppSetup :: (AppConstraints m, Executor e) => e -> RunCtx EventX -> Options -> m (Either ExitCode Int)
myAppSetup exec run_ctx _opts = do
    let log_ctx = run_ctx ^. runCtxLogger

    -- TODO eliminate Either
    Right logger_stdout <- newConsoleOutput LogLevelAll LogFormatLine LogOutputStdOut

    addLogger log_ctx logger_stdout
    logEvent run_ctx LogLevelTerse (EventB 1)

    logExecutorFsEntries run_ctx exec

    addSignalHandler run_ctx [Posix.sigHUP, Posix.sigTERM, Posix.sigINT] sigHandler
    raiseSignal run_ctx Posix.sigHUP

    --pure (Left (ExitFailure 10))
    pure (Right 100)

heartbeat :: AppConstraints m => RunCtx EventX -> m ()
heartbeat run_ctx = do
  --  logEvent run_ctx LogLevelTerse (EventA "1" "2")
    liftIO $ threadDelay 1000000
    heartbeat run_ctx

myAppMain :: (AppConstraints m) => RunCtx EventX -> v -> m ()
myAppMain run_ctx _ = do
    heartbeat_h <- liftIO $ async (heartbeat run_ctx)
    ch          <- listenEventQueue run_ctx
    loop ch heartbeat_h
  where
    cleanup heartbeat_h = do
        liftIO $ cancel heartbeat_h

    loop ch heartbeat_h = do
        maybe_ev <- readEventQueue ch
        case maybe_ev of
            Nothing                             -> pass
            Just (LogEventWithDetails _ _ _ ev) -> case ev of
                LogCmdAddAMFEv ev_amf -> do
                    case ev_amf of
                        (AMFEvSigReceived (UnixSignal sig)) -> do
                            if
                                | sig == Posix.sigHUP -> do
                                    loop ch heartbeat_h
                                | sig == Posix.sigINT -> do
                                    cleanup heartbeat_h
                                    pass
                                | sig == Posix.sigTERM -> do
                                    cleanup heartbeat_h
                                    pass
                                | otherwise -> loop ch heartbeat_h
                        _ -> loop ch heartbeat_h
                ev2 -> do
                    print ev2
                    loop ch heartbeat_h

myAppFinish :: (AppConstraints m) => RunCtx e -> v -> m ()
myAppFinish _run_ctx _ = do
    pass


--------------------------------------------------------------------------------

main :: IO ()
main = parseOpts >>= chooseExecutor
  where
    chooseExecutor opts@(Options DaemonTypeTraditional) = runDaemon myAppSetup myAppMain myAppFinish opts
    chooseExecutor (     Options DaemonTypeKubernetes ) = pass
