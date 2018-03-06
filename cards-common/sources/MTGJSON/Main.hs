{-# LANGUAGE OverloadedStrings #-}

{-|

-}
module MTGJSON.Main where

import MTGJSON.Extra
import MTGJSON
--import MTGJSON.AllSets.Object
--import MTGJSON.AllSets.Lens

import Control.Lens

import qualified Data.Map as Map
--import Control.DeepSeq

--import Control.Exception

----------------------------------------

{-|


TODO:

@

> readDataFile AllDataFile >>= (pSetsObject>return) >>= evaluate 
Left "Error in $.UNH.cards[15].cmc: expected Natural, encountered floating number 0.5"




> valids ^. _Editions . to length
74

> (invalids, valids) = validateEditions os

> vs = valids ^.. _Editions . traverse . edition_name . _EditionName
> vs & traverse_ (toS > putStrLn)
Fifth Dawn
Aether Revolt
Amonkhet
Shards of Alara
Alliances
Apocalypse
Alara Reborn
Avacyn Restored
Battle for Zendikar
Born of the Gods
Betrayers of Kamigawa
Champions of Kamigawa
Conflux
Coldsnap
Dragon's Maze
Dissension
Dark Ascension
Darksteel
Dragons of Tarkir
Eldritch Moon
Eventide
Exodus
Fate Reforged
Future Sight
Guildpact
Gatecrash
Hour of Devastation
Ice Age
Invasion
Innistrad
Journey into Nyx
Judgment
Kaladesh
Khans of Tarkir
Legions
Lorwyn
Mirrodin Besieged
Mirage
Mercadian Masques
Morningtide
Mirrodin
Nemesis
New Phyrexia
Odyssey
Oath of the Gatewatch
Onslaught
Prophecy
Planar Chaos
Planeshift
Ravnica: City of Guilds
Rivals of Ixalan
Rise of the Eldrazi
Return to Ravnica
Scourge
Shadowmoor
Shadows over Innistrad
Saviors of Kamigawa
Scars of Mirrodin
Stronghold
Theros
Tempest
Torment
Time Spiral "Timeshifted"
Time Spiral
Urza's Destiny
Urza's Legacy
Urza's Saga
Visions
Welcome Deck 2016
Welcome Deck 2017
Weatherlight
Worldwake
Ixalan
Zendikar

> length invalids
146

> invalids ^.. traverse . setObject_name
From the Vault: Exiled
From the Vault: Relics
From the Vault: Legends
From the Vault: Realms
From the Vault: Twenty
From the Vault: Annihilation (2014)
From the Vault: Angels
From the Vault: Lore
From the Vault: Transform
Vanguard
Vintage Masters
15th Anniversary
Two-Headed Giant Tournament
Asia Pacific Land Program
Arena League
Celebration
Champs and States
Dragon Con
European Land Program
Friday Night Magic
Grand Prix
Guru
Gateway
Happy Holidays
Judge Gift Program
Legend Membership
Launch Parties
Media Inserts
Magic Game Day
Magic Player Rewards
Portal Demo Game
Prerelease Events
Pro Tour
Release Events
Summer of Magic
Super Series
World Magic Cup Qualifiers
Worlds
Wizards of the Coast Online Store
Wizards Play Network

> is = invalids & traverse . setObject_cards .~ []
> is & traverse_ print

> isRealEdition t = t `elem` (realEditionNames ^.. _EditionName)
> rs = is & traverse . setObject_name
rs & traverse_ print



@

-}

main :: IO ()
main = do
  putStrLn ""
  
  bSetsMetadata <- readDataFile SetsDataFile
  --TODO filesystem requires `backend`-only
  let theSets = pSetsMetadata bSetsMetadata
  print theSets
  
  nothing


----------------------------------------
{-|

@
Just os <- parseSetsFile
@

-}
parseSetsFile :: IO (Maybe [SetObject])
parseSetsFile
    = readDataFile AllDataFile
  >>= ( go
      > return
      )
  >>= forceIO -- ( force > evaluate )
  where
  go = pSetsObject
     > preview (_Right . _SetsObject . to Map.elems)

{-|

@
(invalids, valids) <- validateSetsFile
@

-}
validateSetsFile :: IO ([SetObject], Editions)
validateSetsFile
    = parseSetsFile
  >>= maybe2throw
  >>= (go > forceIO)
  where
  go = validateEditions

