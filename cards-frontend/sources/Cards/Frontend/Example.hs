{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference

{-| 

<https://github.com/reflex-frp/reflex-platform/blob/develop/README.md>

<https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md>

-}
module Cards.Frontend.Example where

import Cards.Frontend()

import Reflex
import Reflex.Dom

{-
import Language.Javascript.JSaddle.Warp
import Reflex.Dom.Core (mainWidget)
import Reflex.Dom hiding (mainWidget,run)
-}

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Text (pack, unpack, Text)
import qualified Data.Text as T

import Text.Read (readMaybe)
import Control.Applicative ((<*>), (<$>))
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

-- app = display =<< count =<< button "ClickMe"

app = el "div" $ do
  nx <- numberInput
  d <- dropdown Times (constDyn ops) def
  ny <- numberInput
  let values = zipDynWith (,) nx ny
      result = zipDynWith (\o (x,y) -> runOp o <$> x <*> y) (_dropdown_value d) values
      resultText = fmap (pack . show) result
  text " = "
  dynText resultText

numberInput :: (MonadWidget t m) => m (Dynamic t (Maybe Double))
numberInput = do
  let errorState = "style" =: "border-color: red"
      validState = "style" =: "border-color: green"
  rec n <- textInput $ def & textInputConfig_inputType .~ "number"
                           & textInputConfig_initialValue .~ "0"
                           & textInputConfig_attributes .~ attrs
      let result = fmap (readMaybe . unpack) $ _textInput_value n
          attrs  = fmap (maybe errorState (const validState)) result
  return result

data Op = Plus | Minus | Times | Divide deriving (Eq, Ord)

ops :: Map Op Text
ops = [(Plus, "+"), (Minus, "-"), (Times, "*"), (Divide, "/")]

runOp :: Fractional a => Op -> a -> a -> a
runOp = \case
            Plus -> (+)
            Minus -> (-)
            Times -> (*)
            Divide -> (/)

