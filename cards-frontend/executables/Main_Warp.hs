
{-|

TODO This module isolates compiler-specific conditional-compilation
(i.e. @GHC@ versus @GHCJS@). 

The conditional-compilation conditions on:

* @-DJSADDLE_WARP@ a.k.a @-fjsaddle-warp@
* TODO

-}
module Main (main) where

  -- ( mainWidgetWithFrontend
  -- , __JAVASCRIPT_RUNNER__
  -- ) where

import Cards.Frontend.Main  (mainWithRunner)
import Cards.Frontend.Types
-- (Runner, SomeWidget(..), Frontend(..), JAVASCRIPT_RUNNER(..))

--import           Reflex.Dom      hiding (mainWidgetWithHead)
import qualified Reflex.Dom.Core        (mainWidgetWithHead)

import qualified Language.Javascript.JSaddle.Warp as JSaddleWarp

--import Prelude

----------------------------------------

main :: IO ()
main = mainWithRunner mainWidgetWithFrontend

mainWidgetWithFrontend :: Runner
mainWidgetWithFrontend (Frontend (SomeWidget wHead) wBodyWithRunner)
  = case wBodyWithRunner __JAVASCRIPT_RUNNER__ of
      SomeWidget wBody -> do
        JSaddleWarp.run port_jsaddle_warp $ do
            Reflex.Dom.Core.mainWidgetWithHead
                wHead
                wBody

port_jsaddle_warp :: Int
port_jsaddle_warp = 3911

-- | the current executable's runner.
-- like @System.os@, this is constant within the program's execution.
__JAVASCRIPT_RUNNER__ :: JAVASCRIPT_RUNNER
__JAVASCRIPT_RUNNER__ = JSADDLEWARP

----------------------------------------
