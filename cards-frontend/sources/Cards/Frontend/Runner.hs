
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
--import Cards.Frontend.Extra (IO_) 
import Cards.Frontend.Types
 (Runner, Frontend(..), JAVASCRIPT_RUNNER(..), displayJavascriptRunner)

import Data.Text (Text)

--------------------
#ifdef ghcjs_HOST_OS
import Reflex.Dom (mainWidgetWithHead)
#else
#ifdef JSADDLE_WARP
import Reflex.Dom.Core (mainWidgetWithHead)
import qualified Language.Javascript.JSaddle.Warp as JSaddleWarp (run)
#else
import Reflex.Dom.Core (mainWidgetWithHead)
import Language.Javascript.JSaddle.WebKitGTK as JSaddleGTK (run) -- TODO
#endif
#endif
--------------------
 
import Prelude.Spiros hiding (Text)

----------------------------------------

__JAVASCRIPT_RUNNER_TEXT__ :: Text
__JAVASCRIPT_RUNNER_TEXT__
  = displayJavascriptRunner __JAVASCRIPT_RUNNER__

----------------------------------------
-- conditional compilation...

mainWidgetWithFrontend :: Runner
#ifdef ghcjs_HOST_OS
mainWidgetWithFrontend = ghcjs_main
#else
mainWidgetWithFrontend = ghc_main
#endif

#ifdef ghcjs_HOST_OS
ghcjs_main :: Runner
ghcjs_main Frontend{..} = do -- liftIO $ do
    mainWidgetWithHead wHead wBody
#else
#ifdef JSADDLE_WARP
ghc_main :: Runner
ghc_main Frontend{..}
  = JSaddleWarp.run __JSADDLE_WARP_PORT__ $ do -- liftIO $ do
      liftIO $ mainWidgetWithHead wHead wBody
#else
ghc_main :: Runner
ghc_main Frontend{..}
  = JSaddleGTK.run $ do
      mainWidgetWithHead wHead wBody
#endif
#endif

#ifdef ghcjs_HOST_OS
#else
#ifdef JSADDLE_WARP
__JSADDLE_WARP_PORT__ :: Int
__JSADDLE_WARP_PORT__ = 3911
#else
#endif
#endif

-- | the current executable's runner.
-- like @System.os@, this is constant within the program's execution.
__JAVASCRIPT_RUNNER__ :: JAVASCRIPT_RUNNER
#ifdef ghcjs_HOST_OS
-- #ifndef JSADDLE_WARP
__JAVASCRIPT_RUNNER__ = BROWSER -- WEBKITGTK
#else
__JAVASCRIPT_RUNNER__ = JSADDLEWARP
#endif

----------------------------------------

{-


-- mainWidgetWith
--   :: () -- (MonadJSM m)
--   => Frontend
--   -> IO_


-- #ifdef ghcjs_HOST_OS
-- ghcjs_main :: Runner
-- ghcjs_main Frontend{wHead,wBody} = liftIO $ do
--   mainWidgetWithHead wHead wBody
-- #else
-- ghc_main :: Runner
-- ghc_main Frontend{wHead,wBody} = liftIO $ do
--   JSaddleWarp.run __JSADDLE_WARP_PORT__ $ do
--       liftIO $ mainWidgetWithHead wHead wBody
-- #endif
--  -- Frontend -> IO_

-}


{-NOTES

mainWidgetWithHead :: (forall x. Widget x ()) -> (forall x. Widget x ()) -> IO ()

-}
