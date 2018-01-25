{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference

{-| 

<https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md>

-}
module Cards.Frontend.Example where

import Cards.Frontend()

import Reflex.Dom

{-
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core (mainWidget)
import Reflex.Dom hiding (mainWidget,run)
-}

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Monoid
-- import System.Environment

----------------------------------------

main :: IO ()

-- main = run 3911 $ mainWidget app

main = mainWidget app

----------------------------------------

{-

@
type Widget x =
  PostBuildT
    Spider
    (ImmediateDomBuilderT
       Spider (WithWebView x (PerformEventT Spider (SpiderHost Global))))
@

-}

app :: Widget t ()
app = display =<< count =<< button "ClickMe"

