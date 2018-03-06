{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTGJSON.AllSets.Enums.Supertype where

import MTGJSON.Extra

import Control.Lens (makePrisms)

----------------------------------------

newtype Supertype = Supertype Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''Supertype

----------------------------------------

basicSupertype :: Supertype
basicSupertype = "Basic"

legendarySupertype :: Supertype
legendarySupertype = "Legendary"

snowSupertype ::Supertype
snowSupertype = "Snow"

ongoingSupertype :: Supertype
ongoingSupertype = "Ongoing"

worldSupertype :: Supertype
worldSupertype = "World"
 
----------------------------------------
