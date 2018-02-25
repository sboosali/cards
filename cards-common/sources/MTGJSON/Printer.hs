{-# LANGUAGE OverloadedStrings #-}

{-|

the inverse/opposite of @"MTGJSON.Parser"@. 

-}
module MTGJSON.Printer
 ( module MTGJSON.Printer
 , module MTGJSON.Printer.Finite
 ) where

import MTGJSON.Printer.Finite

import MTGJSON.Extra
import MTGJSON.Types

import qualified Data.Text.Lazy as T

import Prelude.Spiros hiding (P)

----------------------------------------

displayChromatic :: Print Chromatic
displayChromatic (Chromatic cs)
  = cs
  & fmap displayChroma
  & T.intercalate "" 

displayManaCost :: (Show i) => Print (ManaCost i)
displayManaCost = \case
  ManaCost mana -> mana & maybe "" displayManaSymbols

displayManaSymbols :: (Show i) => Print (ManaSymbols i)
displayManaSymbols (ManaSymbols symbols) = t
    where
    t  = ts & T.intercalate ""         -- e.g. "{2}{U}{G}"
    ts = symbols <&> displayManaSymbol -- e.g. ["{2}","{U}","{G}"]

----------------------------------------

