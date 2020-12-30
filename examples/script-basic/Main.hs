module Main where

import           AMF.Prelude.Script      hiding ( (</>) )

import           Turtle                         ( (</>)
                                                , view
                                                )
import           Turtle.Prelude

--------------------------------------------------------------------------------

myAppMain _exec _run_ctx _opts _st = do
    -- sh like operations and logging
    let root = "/tmp" </> "amf" </> "logs"
    mktree root

    -- run tasks in parallel
    view (parallel [sleep 3 >> date, date, date])

    pass

--------------------------------------------------------------------------------

app :: (AllAppConstraints m) => AppSpec m e ev NoEvent NoEnv NoOpts NoConfig ()
app = defaultAppSpec { appName = "amf-script-basic", appSetup = noAppSetup, appMain = myAppMain, appEnd = noAppFinish }

main :: IO ()
main = runAppSpecAsScript app
