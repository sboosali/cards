{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE CPP #-}

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
--import Prelude.Spiros hiding (Text,div)

import Reflex hiding (Query)

#ifdef JSADDLE_WARP
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core        (mainWidgetWithHead)
import Reflex.Dom      hiding (mainWidgetWithHead,run,Query)
#else
import Reflex.Dom      hiding                (Query)
#endif

--import qualified Control.Lens as L

--import qualified Data.Map as Map

--import qualified Data.Text as T
import Data.Text (Text)
--import Data.Text (pack, unpack)

--import Text.Read (readMaybe)
-- import Data.Monoid
-- import System.Environment

----------------------------------------

main :: IO ()

#ifdef JSADDLE_WARP
main = run 3911 $ mainWidgetWithHead wHead wBody'
#else
main = mainWidgetWithHead wHead wBody'
#endif

----------------------------------------

--TODO lol
wBody' = do
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

sLabel :: Text
#ifdef JSADDLE_WARP
sLabel = "ClickMe (jsaddle-warp)"
#else
sLabel = "ClickMe (webkitgtk)"
#endif

----------------------------------------
