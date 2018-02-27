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
import MTGJSON.Known
--import MTGJSON.Types

--import qualified Data.Text.Lazy as T

----------------------------------------

displayManaCost :: (Show i) => Print (ManaCost i)
displayManaCost (ManaCost cs)
  = cs
  & fmap displayManaSymbol
  & intercalate "" 

----------------------------------------
