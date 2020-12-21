module AMF.Executor.Daemon where

import           Relude

-- base
import           System.Exit

-- Hackage
import           Chronos
import           Path
import           Main.Utf8
import           Control.Concurrent.Async
import           Control.Lens

-- local
import           AMF.API
import           AMF.Events
import           AMF.Executor.Common           as Common
import           AMF.Logging
import           AMF.Logging.Types.Level
import           AMF.Types.Environment
import           AMF.Types.Executor



data Traditional = Traditional
    { _appName :: Text
    }
    deriving stock (Generic, Show)


instance Executor Traditional where
    fsDirRoot _ = Just [absdir|/|]

    fsDirMetadata (Traditional app_name) = (parseRelDir (toString ("etc/" <> app_name)))
    fsDirLogs _ = Just [reldir|var/logs|]

    fsFileAppInfo _ = Nothing

    fsDirCache _ = Just [reldir|var/cache|]

    initExec _run_ctx = do
        pn <- getProgName
        pure (Right (Traditional pn))

    finishExec _run_ctx ctx = pure (Right ctx)

{-
runDaemon
    :: (Show ev, Eventable ev)
    => (Traditional -> RunCtx ev -> t1 -> IO (Either ExitCode t2))
    -> (RunCtx ev -> t2 -> IO t3)
    -> (RunCtx ev -> t3 -> IO ())
    -> t1
    -> IO ()
    -}
runDaemon app_setup app_main app_finish opts = withUtf8 $ do
    run_ctx <- Common.init
    let log_ctx = run_ctx ^. runCtxLogger

    log_h   <- startLogger log_ctx
    maybe_t <- initExec run_ctx
    case maybe_t of
        Left err -> do
            print err
            pass
        Right (t :: Traditional) -> do
            (uptime, result) <- stopwatch $ do
                AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvStart amfVersion)
                AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvPhase Setup)
                setup_result <- app_setup t run_ctx opts
                case setup_result of
                    Left ec -> do
                        Relude.exitWith ec
                    Right setup_val -> do
                        AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvPhase Main)
                        app_handle <- async (app_main run_ctx setup_val)

                        main_val         <- wait app_handle

                        AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvPhase Finish)
                        app_finish run_ctx main_val

            AMF.API.logAMFEvent run_ctx LogLevelTerse (AMFEvStop amfVersion uptime)

            stopLogger log_ctx log_h

            pure result
