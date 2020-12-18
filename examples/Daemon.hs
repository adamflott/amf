module Main where

-- prelude
import           Relude

-- base
import           Control.Concurrent             ( threadDelay )
import           System.Exit

-- Hackage
import           Control.Lens
import           Options.Applicative
import qualified System.Posix                  as Posix

-- local
import           AMF.API
import           AMF.Events
import           AMF.Executor.Daemon
import           AMF.Types.Executor


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

sigHandler :: RunCtx -> Posix.Handler
sigHandler run_ctx = Posix.CatchInfo $ \(Posix.SignalInfo s errno si) -> do
    _ <- writeEventQueue (run_ctx ^. runCtxEventIn) (AMFEvSig (UnixSignal s))
    putTextLn "hi from signal"
    print s

    pass

--------------------------------------------------------------------------------

type AppConstraints m = (MonadIO m, MonadTime m, MonadUnixSignals m, MonadEventQueueRead m)

myAppSetup :: (AppConstraints m, Num a, Executor e) => e -> RunCtx -> Options -> m (Either ExitCode a)
myAppSetup exec run_ctx opts = do
    putTextLn "in setup"
    print opts

    print (fsDirRoot exec)
    print (fsDirMetadata exec)

    putTextLn "install sig handler"
    addSignalHandler run_ctx [Posix.sigHUP, Posix.sigTERM, Posix.sigINT] sigHandler
    putTextLn "done installing sig handler"

    liftIO $ Posix.raiseSignal Posix.sigHUP

    liftIO $ threadDelay 1000000
    --pure (Left (ExitFailure 10))
    pure (Right 100)


myAppMain :: (AppConstraints m, Show v) => RunCtx -> v -> m ()
myAppMain run_ctx v = do
    putTextLn "in main"
    print v

    loop (run_ctx ^. runCtxEventOut)
  where
    loop ch_out = do
        maybe_ev <- readEventQueue ch_out
        case maybe_ev of
            Nothing -> do
                pass
            Just ev -> do
                print ev
                case ev of
                    AMFEvSig (UnixSignal sig) -> do
                        if
                            | sig == Posix.sigHUP -> do
                                loop ch_out
                            | sig == Posix.sigINT -> pass
                            | sig == Posix.sigTERM -> pass
                            | otherwise -> loop ch_out
                    _ -> do
                        loop ch_out


myAppFinish :: (AppConstraints m, Show v) => RunCtx -> v -> m ()
myAppFinish _run_ctx v = do
    putTextLn "in finish"
    print v
    pass


--------------------------------------------------------------------------------

main :: IO ()
main = parseOpts >>= chooseExecutor
  where
    chooseExecutor opts@(Options DaemonTypeTraditional) = runDaemon myAppSetup myAppMain myAppFinish opts
    chooseExecutor opts@(Options DaemonTypeKubernetes ) = undefined
