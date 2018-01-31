{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE CPP #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE RankNTypes #-}

{-| This module isolates compiler-specific conditional-compilation
(i.e. @GHC@ versus @GHCJS@). 

It conditions on @-DJSADDLE_WARP@ a.k.a @-fjsaddle-warp@.

-}
module Cards.Frontend.Runner
  ( mainWidgetWith
  , GHCJS_RUNNER
  , __GHCJS_RUNNER__
  , __GHCJS_RUNNER_TEXT__
  ) where

import Cards.Frontend.Extra (SomeWidget)

import Data.Text (Text)

--------------------
#ifdef JSADDLE_WARP
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core                  (mainWidgetWithHead)
#else
import Reflex.Dom                       (mainWidgetWithHead)
#endif
--------------------
 
import Prelude.Spiros hiding (Text)

----------------------------------------

mainWidgetWith :: SomeWidget () -> SomeWidget () -> IO ()

#ifdef JSADDLE_WARP
mainWidgetWith wHead wBody = run 3911 $ do
  mainWidgetWithHead wHead wBody
#else
mainWidgetWith wHead wBody = do
  mainWidgetWithHead wHead wBody
#endif

-- | introspect on the runner.
data GHCJS_RUNNER
  = JSADDLEWARP
  | WEBKITGTK

-- | the current executable's runner.
-- like @System.os@, this is constant within the program's execution.
__GHCJS_RUNNER__ :: GHCJS_RUNNER 
#ifdef JSADDLE_WARP
__GHCJS_RUNNER__ = JSADDLEWARP
#else
__GHCJS_RUNNER__ = WEBKITGTK
#endif

__GHCJS_RUNNER_TEXT__ :: Text
__GHCJS_RUNNER_TEXT__ = displayGhcjsRunner __GHCJS_RUNNER__
  where
  displayGhcjsRunner :: GHCJS_RUNNER -> Text
  displayGhcjsRunner = \case
    JSADDLEWARP -> "jsaddle-warp"
    WEBKITGTK   -> "webkitgtk"

----------------------------------------
