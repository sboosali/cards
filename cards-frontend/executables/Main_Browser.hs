
{-|

TODO This module isolates compiler-specific conditional-compilation
(i.e. @GHC@ versus @GHCJS@). 

-}
module Main (main) where

import Cards.Frontend.Main  (mainWithRunner)
import Cards.Frontend.Types
 (Runner, SomeWidget(..), Frontend(..), JAVASCRIPT_RUNNER(..))

import qualified Reflex.Dom (mainWidgetWithHead)

--import Prelude

----------------------------------------

main :: IO ()
main = mainWithRunner mainWidgetWithFrontend

mainWidgetWithFrontend :: Runner
mainWidgetWithFrontend (Frontend (SomeWidget wHead) wBodyWithRunner)
  = case wBodyWithRunner BROWSER of
      SomeWidget wBody -> do
          Reflex.Dom.mainWidgetWithHead
            wHead
            wBody

----------------------------------------
