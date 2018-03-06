{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTGJSON.AllSets.Enums.Format where

import MTGJSON.Extra

import Control.Lens (makePrisms)

----------------------------------------

newtype Format = Format Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''Format

----------------------------------------

standardFormat :: Format
standardFormat = "standard"

blockFormat :: Format
blockFormat = "block"

extendedFormat :: Format
extendedFormat = "extended"

vintageFormat :: Format
vintageFormat = "vintage"

classicFormat :: Format
classicFormat = "classic"

legacyFormat :: Format
legacyFormat = "legacy"

modernFormat :: Format
modernFormat = "modern"

commanderFormat :: Format
commanderFormat = "commander"

----------------------------------------
