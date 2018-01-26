{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

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


-}
module Cards.Frontend.Example where

import Cards.Frontend()

import Reflex

#ifdef JSADDLE_WARP
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core (mainWidget)
import Reflex.Dom hiding (mainWidget,run)
#else
import Reflex.Dom
#endif

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Text (pack, unpack, Text)
import qualified Data.Text as T

import Text.Read (readMaybe)
import Control.Applicative ((<*>), (<$>))
import Data.Monoid
-- import System.Environment

----------------------------------------

type Query = Text
type Results = [Result]
type Result = Text

type CardDatabase = [Card]
data Card = Card Text Text

----------------------------------------

main :: IO ()

#ifdef JSADDLE_WARP
main = run 3911 $ mainWidget app
#else
main = mainWidget app
#endif

----------------------------------------

{-|

-}
app :: Widget t ()
app = display =<< count =<< button sLabel

sLabel :: Text
#ifdef JSADDLE_WARP
sLabel = "ClickMe (jsaddle-warp)"
#else
sLabel = "ClickMe (webkitgtk)"
#endif

----------------------------------------
