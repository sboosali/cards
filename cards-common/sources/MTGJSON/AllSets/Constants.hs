{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-|

most useful:

* 'standardEditionNames'
* 'seriousEditionNames'
* 'realEditionNames' 

-}
module MTGJSON.AllSets.Constants where

import MTGJSON.Extra

-- import MTGJSON.AllSets.Object as Object
-- import MTGJSON.AllSets.Set    as Edition

import qualified MTGJSON.AllSets.Card as Card

----------------------------------------

{-|

-}
allEditionNames :: [Card.EditionName]
allEditionNames = concat'
  [ realEditionNames
  , fakeEditionNames
  ]

{-|

-}
fakeEditionNames :: [Card.EditionName]
fakeEditionNames = concat'
  [ 
  ]

----------------------------------------

{-|

-}
realEditionNames :: [Card.EditionName]
realEditionNames = concat'
  [ seriousEditionNames
  , sillyEditionNames
  ]

{-| the @Un-sets@. 

-}
sillyEditionNames :: [Card.EditionName]
sillyEditionNames =
  [ "Unglued"
  , "Unhinged"
  , "Unstable"
  ]

{-|

-}
seriousEditionNames :: [Card.EditionName]
seriousEditionNames = concat'
  [ standardEditionNames
  , supplementalEditionNames
  ]

----------------------------------------

{-|

-}
supplementalEditionNames :: [Card.EditionName]
supplementalEditionNames = concat'
  [ portalSetNames
  , mastersSetNames

  , commanderSetNames
  , archenemySetNames
  , conspiracySetNames
  , planechaseSetNames

  , miscellaneousReprintEditionNames
  , vanguardSetNames
  ]

{-|

-}
commanderSetNames :: [Card.EditionName]
commanderSetNames =
  [ "Magic: The Gathering-Commander"
  , "Commander 2013 Edition"
  , "Commander 2014"
  , "Commander 2015"
  , "Commander 2016"
  , "Commander 2017"
  ]

{-|

-}
archenemySetNames :: [Card.EditionName]
archenemySetNames =
  [ "Archenemy"
  ]

{-|

-}
conspiracySetNames :: [Card.EditionName]
conspiracySetNames =
  [ "Magic: The Gathering—Conspiracy"
  , "Conspiracy: Take the Crown"
  ]

{-|

-}
mastersSetNames :: [Card.EditionName]
mastersSetNames =
  [ "Vintage Masters"
  , "Eternal Masters"
  , "Iconic Masters"
  , "Modern Masters"
  , "Modern Masters 2015 Edition"
  , "Modern Masters 2017 Edition"
  ]

{-|

-}
vanguardSetNames :: [Card.EditionName]
vanguardSetNames =
  [ "Vanguard"
  ]

{-|

-}
portalSetNames :: [Card.EditionName]
portalSetNames =
  [ "Portal"
  , "Portal Second Age"
  , "Portal Three Kingdoms"
  ]

{-|

-}
planechaseSetNames :: [Card.EditionName]
planechaseSetNames =
  [ "Planechase"
  , "Planechase 2012 Edition"
  ]
  
{-|

-}
miscellaneousReprintEditionNames :: [Card.EditionName]
miscellaneousReprintEditionNames =
  [ "Tempest Remastered"
  ]

----------------------------------------

{-|

-}
standardEditionNames :: [Card.EditionName]
standardEditionNames = concat'
  [ expansionSetNames
  , coreSetNames
  , miscellaneousStandardEditionNames
  ]

{-|

-}
coreSetNames :: [Card.EditionName]
coreSetNames =
  [ "Limited Edition Alpha"
  , "Limited Edition Beta"
  , "Unlimited Edition"
  
  , "Fourth Edition"
  , "Fifth Edition"
  , "Classic Sixth Edition"
  , "Seventh Edition"
  , "Eighth Edition"
  , "Tenth Edition"
  , "Ninth Edition"

  , "Magic 2010"
  , "Magic 2011"
  , "Magic 2012"
  , "Magic 2013"
  , "Magic 2014 Core Set"
  , "Magic 2015 Core Set"
  
  , "Magic Origins"
  ]

{-|

-}
miscellaneousStandardEditionNames :: [Card.EditionName]
miscellaneousStandardEditionNames =
  [ "Time Spiral \"Timeshifted\""
  ]

{-|

-}
expansionSetNames :: [Card.EditionName]
expansionSetNames =
  [ "Aether Revolt"
  , "Alara Reborn"
  , "Alliances"
  , "Amonkhet"
  , "Antiquities"
  , "Apocalypse"
  , "Arabian Nights"
  , "Avacyn Restored"
  , "Battle for Zendikar"
  , "Betrayers of Kamigawa"
  , "Born of the Gods"
  , "Champions of Kamigawa"
  , "Coldsnap"
  , "Conflux"
  , "Dark Ascension"
  , "Darksteel"
  , "Dissension"
  , "Dragon's Maze"
  , "Dragons of Tarkir"
  , "Eldritch Moon"
  , "Eventide"
  , "Exodus"
  , "Fallen Empires"
  , "Fate Reforged"
  , "Fifth Dawn"
  , "Future Sight"
  , "Gatecrash"
  , "Guildpact"
  , "Homelands"
  , "Hour of Devastation"
  , "Ice Age"
  , "Innistrad"
  , "Invasion"
  , "Ixalan"
  , "Journey into Nyx"
  , "Judgment"
  , "Kaladesh"
  , "Khans of Tarkir"
  , "Legends"
  , "Legions"
  , "Lorwyn"
  , "Mercadian Masques"
  , "Mirage"
  , "Mirrodin"
  , "Mirrodin Besieged"
  , "Morningtide"
  , "Nemesis"
  , "New Phyrexia"
  , "Oath of the Gatewatch"
  , "Odyssey"
  , "Onslaught"
  , "Planar Chaos"
  , "Planeshift"
  , "Prophecy"
  , "Ravnica: City of Guilds"
  , "Return to Ravnica"
  , "Revised Edition"
  , "Rise of the Eldrazi"
  , "Rivals of Ixalan"
  , "Saviors of Kamigawa"
  , "Scars of Mirrodin"
  , "Scourge"
  , "Shadowmoor"
  , "Shadows over Innistrad"
  , "Shards of Alara"
  , "Stronghold"
  , "Tempest"
  , "The Dark"
  , "Theros"
  , "Time Spiral"
  , "Torment"
  , "Urza's Destiny"
  , "Urza's Legacy"
  , "Urza's Saga"
  , "Visions"
  , "Weatherlight"
  , "Worldwake"
  , "Zendikar"
  ]

----------------------------------------
