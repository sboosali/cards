{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTGJSON.AllSets.Enums.Block where

import MTGJSON.Extra

import MTGJSON.AllSets.Enums.Edition

import Control.Lens (makeLenses, makePrisms)

----------------------------------------

newtype BlockName = BlockName Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''BlockName

----------------------------------------

data BlockInfo = BlockInfo
 { _blockAbbreviation :: Text
 , _blockDescription  :: Text
 , _blockBlocks        :: [EditionName]
 --, _blockLanguages    :: [Language] --NOTE a `Set` 
 } deriving (Show,Read,Eq,Ord,Generic)

instance NFData   BlockInfo
instance Hashable BlockInfo

makeLenses ''BlockInfo

----------------------------------------

mirageBlock :: BlockName
mirageBlock = "Mirage"

rathBlock :: BlockName
rathBlock = "Rath"

urzaBlock :: BlockName
urzaBlock = "Urza"

masquesBlock :: BlockName
masquesBlock = "Masques"

invasionBlock :: BlockName
invasionBlock = "Invasion"

odysseyBlock :: BlockName
odysseyBlock = "Odyssey"

onslaughtBlock :: BlockName
onslaughtBlock = "Onslaught"

mirrodinBlock :: BlockName
mirrodinBlock = "Mirrodin"

kamigawaBlock :: BlockName
kamigawaBlock = "Kamigawa"

ravnicaBlock :: BlockName
ravnicaBlock = "Ravnica"

iceageBlock :: BlockName
iceageBlock = "Ice Age"

timespiralBlock :: BlockName
timespiralBlock = "Time Spiral"

lorwynBlock :: BlockName
lorwynBlock = "Lorwyn"

shadowmoorBlock :: BlockName
shadowmoorBlock = "Shadowmoor"

alaraBlock :: BlockName
alaraBlock = "Shards Of Alara"

zendikarBlock :: BlockName
zendikarBlock = "Zendikar"

scarsBlock :: BlockName
scarsBlock = "Scars Of Mirrodin"

innistradBlock :: BlockName
innistradBlock = "Innistrad"

ravnicaReturnBlock :: BlockName
ravnicaReturnBlock = "Return To Ravnica"

therosBlock :: BlockName
therosBlock = "Theros"

khansBlock :: BlockName
khansBlock = "Khans Of Tarkir"

zendikarReturnBlock :: BlockName
zendikarReturnBlock = "Battle For Zendikar"

shadowsBlock :: BlockName
shadowsBlock = "Shadows Over Innistrad"

kaladeshBlock :: BlockName
kaladeshBlock = "Kaladesh"

amonkhetBlock :: BlockName
amonkhetBlock = "Amonkhet"

ixalanBlock :: BlockName
ixalanBlock = "Ixalan"

----------------------------------------

antediluvianPseudoBlock :: BlockName
antediluvianPseudoBlock = "Antediluvian Sets"

ordinalPseudoBlock :: BlockName
ordinalPseudoBlock = "Ordinal Core Sets"

cardinalPseudoBlock :: BlockName
cardinalPseudoBlock = "Cardinal Core Sets"

----------------------------------------
