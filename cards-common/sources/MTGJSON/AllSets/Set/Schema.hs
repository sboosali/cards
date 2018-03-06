{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

{-|

Refines "MTGJSON.AllSets.Object" with: @Date@s, self-documenting Bool-like enums, and by dropping redundant (non-semantic) @Maybe@s (while keeping semantically-relevant @Maybe@s).

-}
module MTGJSON.AllSets.Set.Schema where

import MTGJSON.Extra
import MTGJSON.AllSets.Orphans()

--import MTGJSON.AllSets.Card.Schema
import MTGJSON.AllSets.Enums
import MTGJSON.AllSets.Booster

import "thyme" Data.Thyme.Calendar

----------------------------------------

{-| 

-}
newtype Editions = Editions [Edition]
  deriving (Show,Eq,Ord,Generic,NFData,Hashable) -- IsList) 

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

  , _Edition_block              :: Maybe BlockName
    -- ^ e.g. "Masques"
    -- The block this set is in.
    -- Some sets have no block, like the Core Sets.  

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

-- | naming: "whether 'OfflineToo' is @True@". 
toWhetherOffline :: Bool -> WhetherOffline
toWhetherOffline = \case
  False -> OnlineOnly
  True  -> OfflineToo

----------------------------------------

