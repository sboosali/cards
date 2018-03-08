{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|


-}
module MTGJSON.AllSets.Booster where

import MTGJSON.Extra 
import MTGJSON.AllSets.Enums.Rarity

import qualified "distribution" Data.Distribution as D 
import           "distribution" Data.Distribution (Distribution,Probability)

import qualified Data.Map as Map
import           Data.Map (Map)
--import qualified Data.Set as Set
import           Data.Set (Set)

----------------------------------------

{-| 

e.g. 'defaultBooster' is:

@
[
      [
        (7/8) "rare",
        (1/8) "mythic rare"
      ],
      "uncommon",
      "uncommon",
      "uncommon",
      "common",
      "common",
      "common",
      "common",
      "common",
      "common",
      "common",
      "common",
      "common",
      "common",
]
@

-}
newtype Booster = Booster
 { getBooster :: [BoosterSlot]
 }
 deriving (Show,Eq,Ord,Generic)

instance NFData     Booster
instance Hashable   Booster

{-| @fromList@ is 'defaultBooster' on @[]@ and 'list2booster' otherwise. 
-}
instance IsList Booster where
  type Item Booster = BoosterSlot
  toList   = getBooster
  fromList = \case
    [] -> defaultBooster
    xs -> toBooster xs

-- | @= 'defaultBooster'@
instance Default Booster where def = defaultBooster

----------------------------------------

{-| 


e.g.

@
[
  "rare",
  "mythic rare",
]
@

The booster slots from are actually relevant for drafting, from @AllSets-x.json@:

@
["mythic rare","rare"]
["rare"]
["uncommon"]
["common"]
["land"]

["foil common","foil mythic rare","foil rare","foil uncommon"]

["double faced"]
["common","double faced mythic rare","double faced rare"]
["double faced common","double faced uncommon"]

["draft-matters"]

["power nine"]

["rare","timeshifted rare"]
["uncommon","timeshifted uncommon"]
["common","timeshifted common"]
["timeshifted purple"]

["urza land"]
@

-}
newtype BoosterSlot = BoosterSlot
  { getBoosterSlot :: (Distribution Text)
  } 
  deriving (Show,Eq,Ord,Generic)

{-| 
-}
instance IsList BoosterSlot where
  type Item BoosterSlot = (Text,Probability)
  toList   = getBoosterSlot > D.toList
  fromList = toBoosterSlot

instance NFData     BoosterSlot where
  rnf :: BoosterSlot -> ()
  rnf (BoosterSlot d) = (rnf (D.toMap d))
    --NOTE `D.toMap` is the actual accessor, thus it should return a reference to the internal Map itself. 
    
    -- where
    -- m = D.toMap d
    -- m' = rnf m

instance Hashable   BoosterSlot where
  hashWithSalt salt (BoosterSlot d) = hashWithSalt salt i
    where
   i = d & D.toList 

-- instance NFData     BoosterSlot
-- instance Hashable   BoosterSlot

-- | @= 'defaultBoosterSlot'@
instance Default BoosterSlot where def = defaultBoosterSlot

----------------------------------------

{-| 15 'BoosterSlot's total:

* 1 'rareSlot' (biased towards @"rare"@ over @"mythic"@)
* 3 'uncommonSlot's (uniform\/singluar)
* 11 'commonSlot's (uniform\/singluar)

The distributions of the officially printed boosters vary from this simplification, and for each set. Factors include:

* the size of the set;
* sampling without replacement (which is related to the following);
* the configuration of the "print sheet", e.g. @Ravenous Chupacabra@ and @Golden Demise@ aren't on the same print sheet (afaik?) and thus shouldn't share two of the three 'uncommonSlot's;
* the presence of newer raritities, like @"mythic"@; 
* the presence of special raritities, like for @Wastes@;
* the presence of special slots, e.g. @foil@, @double-faced@, or @timeshifted@ cards,
which each take one slot per pack; 
* etc.



-}
defaultBooster :: Booster
defaultBooster = Booster $ (concat :: forall a. [[a]] -> [a])
 [ replicate 1  rareSlot
 , replicate 3  uncommonSlot
 , replicate 11 commonSlot
 ]

rareSlot :: BoosterSlot
rareSlot =
  [ "rare"   -: 7
  , "mythic" -: 1
  ]

uncommonSlot :: BoosterSlot
uncommonSlot =
  [ "uncommon" -: 1
  ]

commonSlot :: BoosterSlot
commonSlot =
  [ "common" -: 1
  ]

-- | uses 'defaultBoosterSlotProbabilities'. 
toBooster :: [BoosterSlot] -> Booster
toBooster
  = fmap recalibrateBoosterSlot
  > Booster
  where

{- | @= 'recalibrateBoosterSlotWith' 'defaultBoosterSlotProbabilities'@

This is necessary because @mtgjson@'s @booster@ field is incomplete:each slot holds only the categories (e.g. @["rare","mythic"]@) and not their relative probabilities (e.g. which would look like @{"rare": 7/8, "mythic": 1/8}@).

-}
recalibrateBoosterSlot :: BoosterSlot -> BoosterSlot
recalibrateBoosterSlot = recalibrateBoosterSlotWith defaultBoosterSlotProbabilities

{- |

Replaces the uniformly-distributed @rare-or-mythic@ slot with 'rareSlot' (among others, see the source). 

-}
defaultBoosterSlotProbabilities
  :: Map (Set Text) BoosterSlot
defaultBoosterSlotProbabilities = makeBoosterSlotProbabilities
  [ rareSlot
  , [ "foil"-: 52, "power nine"-: 1 ]
 -- , [ "foil common", "foil mythic rare","foil rare","foil uncommon"]
  ]

{-| (helper)

e.g.

>>> :set -XOverloadedStrings
>>> :set -XOverloadedLists
>>> makeBoosterSlotProbabilities [ ["common"-: 1], [ "mythic"-: 1, "rare"-: 7 ] ]
fromList [(fromList ["common"],BoosterSlot {getBoosterSlot = fromList [("common",1 % 1)]}),(fromList ["mythic","rare"],BoosterSlot {getBoosterSlot = fromList [("mythic",1 % 8),("rare",7 % 8)]})]

-}
makeBoosterSlotProbabilities
  :: [BoosterSlot]
  -> Map (Set Text) BoosterSlot
makeBoosterSlotProbabilities
 = fmap (go &&& id)
 > Map.fromList
 where
 go :: BoosterSlot -> Set Text
 go = getBoosterSlot > D.support 

{- | 
-}
recalibrateBoosterSlotWith
  :: Map (Set Text) BoosterSlot
  -> BoosterSlot
  -> BoosterSlot
recalibrateBoosterSlotWith probabilities = go
  where
  go x@(BoosterSlot d) = y
      where
      y = fromMaybe x $ Map.lookup ts probabilities
      ts = D.support d

----------------------------------------

toBoosterSlot :: [Item BoosterSlot] -> BoosterSlot
toBoosterSlot = D.fromList > BoosterSlot

uniformBoosterSlot :: [Text] -> BoosterSlot
uniformBoosterSlot = D.uniform > BoosterSlot

-- uniformBoosterSlot xs = toBoosterSlot ps
--   where
--   ps = zip xs (repeat (1 % n))
--   n  = length xs

singletonBoosterSlot :: Text -> BoosterSlot
singletonBoosterSlot x = toBoosterSlot [(x,1)]

defaultBoosterSlot :: BoosterSlot
defaultBoosterSlot = BoosterSlot $ D.always t
  where
  Rarity t = common

----------------------------------------

makePrisms ''Booster

makePrisms ''BoosterSlot

----------------------------------------

{-NOTES

all unique values of booster slots, from @AllSets-x.json@:

@
["Steamflogger Boss","land"]
["checklist","land"]
["checklist","marketing"]
["common"]
["common","double faced mythic rare","double faced rare"]
["common","timeshifted common"]
["double faced"]
["double faced common","double faced uncommon"]
["draft-matters"]
["foil","power nine"]
["foil common","foil mythic rare","foil rare","foil uncommon"]
["land"]
["marketing"]
["mythic rare","rare"]
["rare"]
["rare","timeshifted rare"]
["rare","uncommon"]
["timeshifted common"]
["timeshifted purple"]
["timeshifted rare","timeshifted uncommon"]
["timeshifted uncommon","uncommon"]
["token"]
["uncommon"]
["urza land"]
@

of which of these are not completely irrelevant for drafting:

@
["Steamflogger Boss","land"]
["common"]
["common","double faced mythic rare","double faced rare"]
["common","timeshifted common"]
["double faced"]
["double faced common","double faced uncommon"]
["draft-matters"]
["foil","power nine"]
["foil common","foil mythic rare","foil rare","foil uncommon"]
["land"]
["mythic rare","rare"]
["rare"]
["rare","timeshifted rare"]
["rare","uncommon"]
["timeshifted common"]
["timeshifted purple"]
["timeshifted rare","timeshifted uncommon"]
["timeshifted uncommon","uncommon"]
["uncommon"]
["urza land"]
@

-}
