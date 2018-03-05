{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

{-|

Refines "MTGJSON.AllSets.Object" with: @Date@s, self-documenting Bool-like enums, and by dropping redundant (non-semantic) @Maybe@s (while keeping semantically-relevant @Maybe@s).

-}
module MTGJSON.AllSets.Set.Schema where

import MTGJSON.Extra -- hiding (ByteString)
import MTGJSON.AllSets.Card.Schema
--import MTGJSON.AllSets.Orphans

import "thyme" Data.Thyme.Calendar

import qualified "distribution" Data.Distribution as D 
import           "distribution" Data.Distribution (Distribution,Probability)

----------------------------------------

{-| 

-}
newtype Editions = Editions [Edition]
  deriving (Show,Eq,Ord,Generic) -- IsList) 

----------------------------------------

-- data SetCodes = SetCodes
--  { primaryCode        :: Text
--  , magicCardsInfoCode :: First Text
--  , gathererCode       :: First Text
--  }

-- getSetCodes :: SetSchema -> SetCodes
-- getSetCodes SetSchema{..} = SetCodes{..}
--  where
--  primaryCode        = _SetSchema_code
--  gathererCode       = _SetSchema_gathererCode
--    & First 
--  magicCardsInfoCode = _SetSchema_magicCardsInfoCode
--    & First

----------------------------------------


{-

e.g.

-}
data Edition = Edition

  { _Edition_name               :: EditionName
    -- ^ "Nemesis",
    -- The name of the set

  , _Edition_codes              :: EditionCodes
    -- ^ 

  , _Edition_block              :: BlockName
    -- ^ e.g. "Masques"
    -- The block this set is in. 

  , _Edition_type               :: EditionType
    -- ^ e.g. "expansion"
    -- The type of set.
    -- One of: "core", "expansion", "reprint", "box", "un", "from the vault", "premium deck", "duel deck", "starter", "commander", "planechase", "archenemy","promo", "vanguard", "masters", "conspiracy", "masterpiece". 

  , _Edition_border             :: Border
    -- ^ "black",
    -- The type of border on the cards,
    -- either "white", "black" or "silver". 

  , _Edition_booster            :: Booster
    -- ^ e.g. @[ "rare", ... ]@ 
    -- Booster contents for this set

  , _Edition_releaseDate        :: Day
    -- ^ "2000-02-14"
    -- When the set was released (YYYY-MM-DD). 
    -- (For promo sets, the date the first card was released).

  , _Edition_onlineOnly         :: WhetherOffline
    -- ^ if the set was only released online (i.e. not in paper). 

  {-
  , _Edition_cards              :: [card]   -- ^ [ {}, {}, {}, ... ]    -- ^ 
  -}
  
  } deriving (Show,Eq,Ord,Generic)

instance NFData     Edition
instance Hashable   Edition

----------------------------------------

newtype BlockName = BlockName Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

newtype EditionType = EditionType Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

----------------------------------------

data EditionCodes = EditionCodes

 { _Edition_primaryCode        :: !(Text)
   -- ^ TODO e.g. "NMS"
   -- The set's abbreviated code

 , _Edition_gathererCode       :: !(Text)
   -- ^ e.g. "NE"
   -- The code that Gatherer uses for the set.
   -- Normally identical to '_Edition_primaryCode'. 

 , _Edition_oldCode            :: !(Text)
   -- ^ e.g. "NEM"
   -- The (deprecated) old-style code, used by some Magic software.
   -- Normally identical to ' _Edition_'gathererCode' (or '_Edition_primaryCode')

 , _Edition_magicCardsInfoCode :: !(Maybe Text)
   -- ^ e.g. "ne"
   -- The code that magiccards.info uses for the set.
   -- @Nothing@ if absent from @magiccards.info@. 
   -- Normally identical to ' _Edition_'gathererCode' (or '_Edition_primaryCode')
 
 } deriving (Show,Read,Eq,Ord,Generic)

instance NFData     EditionCodes
instance Hashable   EditionCodes

instance IsString (EditionCodes ) where
  fromString = fromString > simpleEditionCodes

simpleEditionCodes :: Text -> EditionCodes
simpleEditionCodes t = EditionCodes t t t (Just t) 

----------------------------------------

data WhetherOffline
 = OfflineToo
 | OnlineOnly
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic)

instance NFData     WhetherOffline
instance Hashable   WhetherOffline
instance Enumerable WhetherOffline

-- | @= 'not' >>> 'toWhetherOffline'@
fromOnlineOnly :: Bool -> WhetherOffline
fromOnlineOnly = not > toWhetherOffline

toWhetherOffline :: Bool -> WhetherOffline
toWhetherOffline = \case
  False -> OfflineToo
  True  -> OnlineOnly

----------------------------------------

{-| 

e.g.

@
[
      [
        "rare",
        "mythic rare"
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

{-| 
-}
instance IsList Booster where
  type Item Booster = BoosterSlot
  toList   = getBooster
  fromList = list2booster

list2booster :: [BoosterSlot] -> Booster
list2booster = Booster

{-| 


e.g.

@
[
  "rare",
  "mythic rare"
]
@

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

and of which these are actually relevant for drafting:

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

instance NFData     BoosterSlot where
  rnf :: BoosterSlot -> ()
  rnf (BoosterSlot d) = (rnf (D.toMap d))
    -- where
    -- m = D.toMap d
    -- m' = rnf m

instance Hashable   BoosterSlot where
  hashWithSalt salt (BoosterSlot d) = hashWithSalt salt i
    where
   i = d & D.toList 
  

-- instance NFData     BoosterSlot
-- instance Hashable   BoosterSlot

----------------------------------------
