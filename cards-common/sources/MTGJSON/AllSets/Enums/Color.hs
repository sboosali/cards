{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTGJSON.AllSets.Enums.Color where

import MTGJSON.Extra

import Control.Lens (makePrisms)

----------------------------------------

newtype Color = Color Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''Color

----------------------------------------

white :: Color
white = "White"

blue :: Color
blue = "Blue"

black :: Color
black = "Black"

red :: Color
red = "Red"

green :: Color
green = "Green"

----------------------------------------

