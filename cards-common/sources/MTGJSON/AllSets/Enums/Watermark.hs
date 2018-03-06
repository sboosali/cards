{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

These groups of watermarks have been printed:

* 'colorWatermarks' 
* 'guildWatermarks' from "ravnica" block
* 'wedgeWatermarks' from "khans of tarkir"
* 'dragonlordWatermarks' from "dragons of tarkir"
* 'beseigedWatermarks' from the "mirrodin beseiged" block
* 'unstableWatermarks' from "unstable"
* 'miscellaneousWatermarks' (ungrouped)

-}
module MTGJSON.AllSets.Enums.Watermark where

import MTGJSON.Extra

import Control.Lens (makePrisms)

----------------------------------------

newtype Watermark = Watermark Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''Watermark

----------------------------------------

colorWatermarks :: Set Watermark
colorWatermarks =
  [ whiteWatermark
  , blueWatermark
  , blackWatermark
  , redWatermark
  , greenWatermark
  ]

guildWatermarks :: Set Watermark
guildWatermarks =
  [ azoriusWatermark
  , dimirWatermark
  , rakdosWatermark
  , gruulWatermark
  , selesnyaWatermark
  , orzhovWatermark
  , izzetWatermark
  , golgariWatermark
  , borosWatermark
  , simicWatermark
  ]

wedgeWatermarks :: Set Watermark
wedgeWatermarks =
  [ marduWatermark
  , temurWatermark
  , abzanWatermark
  , jeskaiWatermark
  , sultaiWatermark
  ]

dragonlordWatermarks :: Set Watermark
dragonlordWatermarks =
  [ ojutaiWatermark
  , silumgarWatermark
  , kolaghanWatermark
  , atarkaWatermark
  , dromokaWatermark
  ]

beseigedWatermarks :: Set Watermark
beseigedWatermarks =
  [ mirranWatermark
  , phyrexianWatermark
  ]

unstableWatermarks :: Set Watermark
unstableWatermarks =
  [ agentsOfSNEAKWatermark
  , crossbreedLabsWatermark
  , goblinExplosioneersWatermark
  , leagueOfDastardlyDoomWatermark
  ]

miscellaneousWatermarks :: Set Watermark
miscellaneousWatermarks =
  [ planeswalkerWatermark
  , colorlessWatermark
  ]

----------------------------------------

abzanWatermark :: Watermark
abzanWatermark = "Abzan"

agentsOfSNEAKWatermark :: Watermark
agentsOfSNEAKWatermark = "Agents of S.N.E.A.K."

atarkaWatermark :: Watermark
atarkaWatermark = "Atarka"

azoriusWatermark :: Watermark
azoriusWatermark = "Azorius"

blackWatermark :: Watermark
blackWatermark = "Black"

blueWatermark :: Watermark
blueWatermark = "Blue"

borosWatermark :: Watermark
borosWatermark = "Boros"

colorlessWatermark :: Watermark
colorlessWatermark = "Colorless"

crossbreedLabsWatermark :: Watermark
crossbreedLabsWatermark = "Crossbreed Labs"

dimirWatermark :: Watermark
dimirWatermark = "Dimir"

dromokaWatermark :: Watermark
dromokaWatermark = "Dromoka"

goblinExplosioneersWatermark :: Watermark
goblinExplosioneersWatermark = "Goblin Explosioneers"

golgariWatermark :: Watermark
golgariWatermark = "Golgari"

greenWatermark :: Watermark
greenWatermark = "Green"

gruulWatermark :: Watermark
gruulWatermark = "Gruul"

izzetWatermark :: Watermark
izzetWatermark = "Izzet"

jeskaiWatermark :: Watermark
jeskaiWatermark = "Jeskai"

kolaghanWatermark :: Watermark
kolaghanWatermark = "Kolaghan"

leagueOfDastardlyDoomWatermark :: Watermark
leagueOfDastardlyDoomWatermark = "League of Dastardly Doom"

marduWatermark :: Watermark
marduWatermark = "Mardu"

mirranWatermark :: Watermark
mirranWatermark = "Mirran"

ojutaiWatermark :: Watermark
ojutaiWatermark = "Ojutai"

orderOfTheWidgetWatermark :: Watermark
orderOfTheWidgetWatermark = "Order of the Widget"

orzhovWatermark :: Watermark
orzhovWatermark = "Orzhov"

phyrexianWatermark :: Watermark
phyrexianWatermark = "Phyrexian"

planeswalkerWatermark :: Watermark
planeswalkerWatermark = "Planeswalker"

rakdosWatermark :: Watermark
rakdosWatermark = "Rakdos"

redWatermark :: Watermark
redWatermark = "Red"

selesnyaWatermark :: Watermark
selesnyaWatermark = "Selesnya"

silumgarWatermark :: Watermark
silumgarWatermark = "Silumgar"

simicWatermark :: Watermark
simicWatermark = "Simic"

sultaiWatermark :: Watermark
sultaiWatermark = "Sultai"

temurWatermark :: Watermark
temurWatermark = "Temur"

whiteWatermark :: Watermark
whiteWatermark = "White"

----------------------------------------
