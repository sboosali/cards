
{-# LANGUAGE CPP #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE RankNTypes #-}

{-| This module isolates compiler-specific conditional-compilation
(i.e. @GHC@ versus @GHCJS@). 

The conditional-compilation conditions on:

* @-DJSADDLE_WARP@ a.k.a @-fjsaddle-warp@
* TODO

-}
module Cards.Frontend.Runner
  ( mainWidgetWithFrontend
  , JAVASCRIPT_RUNNER
  , __JAVASCRIPT_RUNNER__
  , __JAVASCRIPT_RUNNER_TEXT__
  ) where

-- import Cards.Frontend.Extra (MonadJSM, IO_, SomeWidget_)

import Cards.Frontend.Extra

import Cards.Frontend.Types
 (Runner, SomeWidget(..), SomeJSaddleWidget(..), Frontend(..), JAVASCRIPT_RUNNER(..), displayJavascriptRunner)

import Data.Text (Text)

--------------------
#ifdef JSADDLE_WARP
import Reflex.Dom.Core (mainWidgetWithHead)
import qualified Language.Javascript.JSaddle.Warp as JSaddleWarp (run)
#else
import Reflex.Dom (mainWidgetWithHead)
#endif
--------------------
 
--import Prelude.Spiros hiding (Text)

----------------------------------------

__JAVASCRIPT_RUNNER_TEXT__ :: Text
__JAVASCRIPT_RUNNER_TEXT__
  = displayJavascriptRunner __JAVASCRIPT_RUNNER__

----------------------------------------
-- conditional compilation...

mainWidgetWithFrontend :: Runner
#ifdef JSADDLE_WARP
mainWidgetWithFrontend = ghc_main
#else
mainWidgetWithFrontend = ghc_or_ghcjs_main
#endif

#ifdef JSADDLE_WARP
ghc_main :: Runner
ghc_main
  (Frontend (SomeWidget wHead) (SomeJSaddleWidget wBody))
  = JSaddleWarp.run __JSADDLE_WARP_PORT__ $ do -- liftIO $ do
      liftIO $ mainWidgetWithHead wHead wBody
#else
ghc_or_ghcjs_main :: Runner --TODO lol
ghc_or_ghcjs_main
  (Frontend (SomeWidget wHead) (SomeJSaddleWidget wBody)) = liftIO $ do
    mainWidgetWithHead wHead wBody
#endif

#ifdef JSADDLE_WARP
__JSADDLE_WARP_PORT__ :: Int
__JSADDLE_WARP_PORT__ = 3911
#endif

-- | the current executable's runner.
-- like @System.os@, this is constant within the program's execution.
__JAVASCRIPT_RUNNER__ :: JAVASCRIPT_RUNNER
#ifdef ghcjs_HOST_OS
__JAVASCRIPT_RUNNER__ = BROWSER -- WEBKITGTK
#else
#ifdef JSADDLE_WARP
__JAVASCRIPT_RUNNER__ = JSADDLEWARP
#else
__JAVASCRIPT_RUNNER__ = NODEJS --TODO??
#endif
#endif

----------------------------------------

{-STUFF

-}


{-NOTES

-- #ifdef ghcjs_HOST_OS


mainWidgetWithHead :: (forall x. Widget x ()) -> (forall x. Widget x ()) -> IO ()

-}
