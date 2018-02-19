{-# LANGUAGE RankNTypes #-}

{-|

-}
module JSaddleWebSocketsReloader
  ( runWith
  , reload  
  ) where

import qualified JSaddleWebSocketsRunner

import Cards.Frontend.Main  (mainWithRunner)
import Cards.Frontend.Types

import Reflex.Dom.Core (Widget)

-- import Spiros.Prelude (IO, FilePath, Int)

----------------------------------------

reload :: IO ()
reload = mainWithRunner mainWidgetWithFrontend

mainWidgetWithFrontend :: Runner
mainWidgetWithFrontend (Frontend (SomeWidget _) wBodyWithRunner)
  = case wBodyWithRunner __JAVASCRIPT_RUNNER__ of
      SomeWidget wBody -> do
        runWith $ do
          wBody

----------------------------------------

runWith 
  :: (forall x. Widget x ())
  -> IO ()
runWith =
  JSaddleWebSocketsRunner.runAt static_directory port_jsaddle_warp

static_directory :: FilePath
static_directory = "static/css"
  
port_jsaddle_warp :: Int
port_jsaddle_warp = 3911

-- | the current executable's runner.
-- like @System.os@, this is constant within the program's execution.
__JAVASCRIPT_RUNNER__ :: JAVASCRIPT_RUNNER
__JAVASCRIPT_RUNNER__ = JSADDLEWEBSOCKETS -- JSADDLEWARP6

----------------------------------------
  
{-

JSaddleWarp.run

JSaddleWarp.debug

-}
