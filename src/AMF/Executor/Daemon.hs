module AMF.Executor.Daemon where

import           Relude

-- Hackage
import           Path
import           Main.Utf8
import           Control.Concurrent.Async
import           Control.Lens

-- local
import           AMF.Types.Executor
import           AMF.Executor.Common           as Common
import           AMF.API
import           AMF.Types.Environment
import           AMF.Events



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


runDaemon app_setup app_main app_finish opts = withUtf8 $ do
    run_ctx <- Common.init
    maybe_t <- initExec run_ctx
    case maybe_t of
        Left err -> do
            print err
            pass
        Right (t :: Traditional) -> do
            print t
            t0           <- getNow
            _            <- writeEventQueue (run_ctx ^. runCtxEventIn) (AMFEvStart t0)
            setup_result <- app_setup t run_ctx opts
            case setup_result of
                Left ec -> do
                    _ <- closeEventQueue (run_ctx ^. runCtxEventIn)
                    Relude.exitWith ec
                Right v -> do
                    app_handle <- async (app_main run_ctx v)
                    vv         <- wait app_handle
                    _          <- closeEventQueue (run_ctx ^. runCtxEventIn)
                    vvv        <- app_finish run_ctx vv
                    pure vvv
