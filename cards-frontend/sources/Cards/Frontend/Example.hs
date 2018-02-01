{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference

{-| 

<https://github.com/reflex-frp/reflex-platform/blob/develop/README.md>

<https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md>

Naming:

* @eText :: Event    t Text@
* @dText :: Dynamic  t Text@
* @bText :: Behavior t Text@

* @cTextArea :: TextAreaConfig@
* @wTextArea :: TextArea@


 (setq-local 
     dante-repl-command-line 
     (list "nix-shell" "/home/sboo/haskell/cards/default.nix" "-A" "shells.ghc" "--run" "cabal new-repl cards-frontend"))

-}
module Cards.Frontend.Example where

import Cards.Frontend
import Cards.Frontend.Extra
import Cards.Frontend.Runner (mainWidgetWith, __GHCJS_RUNNER_TEXT__)
--import Prelude.Spiros hiding (Text,div)

import Reflex hiding (Query)

import Reflex.Dom      hiding (mainWidgetWithHead,run,Query,element)

--import qualified Control.Lens as L

--import qualified Data.Map as Map

--import qualified Data.Text as T
--import Data.Text (Text)
--import Data.Text (pack, unpack)

--import Text.Read (readMaybe)
-- import Data.Monoid
-- import System.Environment

----------------------------------------

main :: IO ()
main = mainWidgetWith wHead wBody'

----------------------------------------

--TODO lol
wBody' = do
  test
  app
  wBody

{-|

@
type Widget x =
  PostBuildT
    Spider
    (ImmediateDomBuilderT
       Spider (WithWebView x (PerformEventT Spider (SpiderHost Global))))
@


@
main = run 3911 $ mainWidget app
@

versus

@
main = mainWidget app
@

-}
--app :: Widget t ()
app = display =<< count_Int =<< button sLabel
 where
-- count_Int :: Event t a -> Event t Int
 count_Int = count & (fmap.fmap.fmap) (id :: Int -> Int)
--app = sLabel & (button >=> display >=> count)
--app = button sLabel >>= count >>= display 
 sLabel = "ClickMe ("<> __GHCJS_RUNNER_TEXT__ <>")"

----------------------------------------

--test :: SomeWidget_  
test = do
  
 (events, _widget) <- element
     "div"
      (pure $ "style" =: style)
      blank -- wMousePosition
      -- NOTE
      -- "thread blocked indefinitely in an MVar operation"

 let eMousemove = events & onMousemove
 let eContent = (eMousemove <&> displayMousePosition)
 
 dContent <- holdDyn "(__,__)" eContent

 let wMousePosition = dynText dContent
 
 wMousePosition

 where
 displayMousePosition (MousePosition x y) = s2t $ show (x,y)
 style = "display:inline-block;width:100px;height:100px;background-color:gray"
 
----------------------------------------
