{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTGJSON.AllSets.Enums.Rarity where

import MTGJSON.Extra

import Control.Lens (makePrisms)

----------------------------------------

newtype Rarity = Rarity Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''Rarity

-- | @= 'common'@
instance Default Rarity where def = common

----------------------------------------

mythic :: Rarity
mythic = "Mythic"

rare :: Rarity
rare = "Rare"

uncommon :: Rarity
uncommon = "Uncommon"

common :: Rarity
common = "Common"

----------------------------------------

mythicAbbreviation :: Char
mythicAbbreviation = 'M'

rareAbbreviation :: Char
rareAbbreviation = 'R'

uncommonAbbreviation :: Char
uncommonAbbreviation = 'U'

commonAbbreviation :: Char
commonAbbreviation = 'C'

{-
mythicAbbreviation :: Rarity
mythicAbbreviation = "M"

rareAbbreviation :: Rarity
rareAbbreviation = "R"

uncommonAbbreviation :: Rarity
uncommonAbbreviation = "U"

commonAbbreviation :: Rarity
commonAbbreviation = "C"
-}

----------------------------------------
