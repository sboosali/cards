{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTGJSON.AllSets.Enums.Symbol where

import MTGJSON.Extra

--import MTGJSON.AllSets.Enums.Mana

import Control.Lens (makePrisms)

----------------------------------------

newtype Symbol = Symbol Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''Symbol

----------------------------------------

tapSymbol :: Symbol
tapSymbol = "T"

untapSymbol :: Symbol
untapSymbol = "Q"

----------------------------------------

loyaltyActivationSymbol :: Integer -> Symbol
loyaltyActivationSymbol i =
  if i >= 0
  then Symbol $ "+" <> s -- {+0}, {+1}, ...
  else Symbol $ "-" <> s -- {-1}, ...
  where
  s = show' (abs i)

----------------------------------------
