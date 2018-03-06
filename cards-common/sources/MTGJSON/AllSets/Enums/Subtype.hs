
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTGJSON.AllSets.Enums.Subtype where

import MTGJSON.Extra

import Control.Lens (makePrisms)

----------------------------------------

newtype Subtype = Subtype Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''Subtype

----------------------------------------

----------------------------------------

{-

data Subtype
 = SpellSubtype         SpellType
 | LandSubtype          LandType
 | ArtifactSubtype      ArtifactType
 | EnchantmentSubtype   EnchantmentType
 | CreatureSubtype      CreatureType
 | PlaneswalkerSubtype  PlaneswalkerType

-}