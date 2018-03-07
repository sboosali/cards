{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

{-|

Accurately represents the raw JSON object of @AllSets-x.json@, with a /completely/ generically derived 'FromJSON' instance. 

-}
module MTGJSON.AllSets.Object.Schema where

import MTGJSON.Extra -- hiding (ByteString)

import MTGJSON.Aeson
import Data.Scientific (Scientific)

import Data.Aeson (eitherDecode) --, eitherDecode') 
import Data.Aeson.Types
 (FromJSON(..),Options(..), SumEncoding(..), defaultOptions, genericParseJSON) 

--import Data.ByteString      (ByteString) 
--import Data.ByteString.Lazy (ByteString) 

----------------------------------------

-- | @= 'eitherDecode' @ 
pSetsObject :: ByteString -> Either String SetsObject
pSetsObject = eitherDecode

-- | @= 'eitherDecode' @ 
pSetsArray :: ByteString -> Either String SetsArray
pSetsArray = eitherDecode

----------------------------------------

{-| 

-}
data SetsObject = SetsObject (Map Text SetObject)
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,FromJSON) -- NOTE containers-Map is not Hashable 

{-| 

-}
data SetsArray = SetsArray [SetObject]
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,FromJSON) -- NOTE containers-Map is not Hashable 

----------------------------------------
-- SetObject

data SetCodes = SetCodes
 { primaryCode        :: Text
 , magicCardsInfoCode :: First Text
 , gathererCode       :: First Text
 }

getSetCodes :: SetObject -> SetCodes
getSetCodes SetObject{..} = SetCodes{..}
 where
 primaryCode        = _SetObject_code
 gathererCode       = _SetObject_gathererCode
   & First 
 magicCardsInfoCode = _SetObject_magicCardsInfoCode
   & First

{-| 

-}
data SetObject = SetObject 
  { _SetObject_name               :: Text  -- ^ "Nemesis",       // The name of the set
  , _SetObject_code               :: Text  -- ^ "NMS",           // The set's abbreviated code
  , _SetObject_gathererCode       :: Maybe Text  -- ^ "NE",            // The code that Gatherer uses for the set. Only present if different than 'code'
  , _SetObject_oldCode            :: Maybe Text  -- ^ "NEM",           // An old style code used by some Magic software. Only present if different than 'gathererCode' and 'code'
 , _SetObject_magicCardsInfoCode :: Maybe Text  -- ^ "ne",            // The code that magiccards.info uses for the set. Only present if magiccards.info has this set
 , _SetObject_releaseDate        :: Maybe Text  -- ^ "2000-02-14"     // When the set was released (YYYY-MM-DD). For promo sets, the date the first card was released.
 , _SetObject_border             :: Maybe Text  -- ^ "black",         // The type of border on the cards, either "white", "black" or "silver"
 , _SetObject_type               :: Text  -- ^ "expansion",     // Type of set. One of: "core", "expansion", "reprint", "box", "un", "from the vault", "premium deck", "duel deck", "starter", "commander", "planechase", "archenemy","promo", "vanguard", "masters", "conspiracy", "masterpiece"
 , _SetObject_block              :: Maybe Text  -- ^ "Masques",       // The block this set is in,
 , _SetObject_onlineOnly         :: Maybe Bool  -- ^ false,           // Present and set to true if the set was only released online
 , _SetObject_booster            :: Maybe MagicBoosterObject  -- ^ [ "rare", ... ], // Booster contents for this set, see below for details
  , _SetObject_cards              :: [CardObject]   -- ^ [ {}, {}, {}, ... ]  
  } deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable )
instance FromJSON SetObject where
  parseJSON = parseJSON_TypePrefix

-- | see 'MagicBoosterSlotObject' 
type MagicBoosterObject = [MagicBoosterSlotObject] 

{-| 

"Each item in the array is either a string representing the type of booster card or an array of strings representing possible types for that booster card" 

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
data MagicBoosterSlotObject 
    = MagicBoosterSlotObjectSingle   Text 
    | MagicBoosterSlotObjectMultiple [Text] 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable )
instance FromJSON MagicBoosterSlotObject where
  parseJSON = genericParseJSON defaultOptions
    { sumEncoding = UntaggedValue
    }

fromMagicBoosterSlotObject :: MagicBoosterSlotObject -> [Text]
fromMagicBoosterSlotObject = \case
 MagicBoosterSlotObjectSingle   x  -> [x] 
 MagicBoosterSlotObjectMultiple xs -> xs

----------------------------------------
-- CardObject

{-| this schema is pretty loose,
which is necessary to be able to parse the full @AllSets-x.json@. 

generalizes CMC from Natural to Scientific for Un-sets.

-}
data CardObject = CardObject 
  { _CardObject_id            :: Text 
  , _CardObject_name          :: Text 
  , _CardObject_layout        :: Maybe Text 
  , _CardObject_names         :: Maybe [Text] 
  , _CardObject_manaCost      :: Maybe Text 
  , _CardObject_cmc           :: Scientific
    -- ^ Un-cards can have non-Natural power\toughness.
    -- 
  , _CardObject_colors        :: Maybe [Text] 
  , _CardObject_colorIdentity :: Maybe [Text] 
  , _CardObject_type          :: Text 
  , _CardObject_supertypes    :: Maybe [Text] 
  , _CardObject_types         :: Maybe [Text]
     -- ^ Un-cards can have no type 
  , _CardObject_subtypes      :: Maybe [Text] 
  , _CardObject_rarity        :: Text 
  , _CardObject_text          :: Maybe Text 
  , _CardObject_flavor        :: Maybe Text 
  , _CardObject_artist        :: Text
  , _CardObject_number        :: Maybe Text
    -- ^ CCN
  , _CardObject_power         :: Maybe Text
    -- ^ Un-cards can have non-integer power\toughness,
    -- and even real cards can have non-natural\non-integer power\toughness,
    -- like @*+1@. 
  , _CardObject_toughness     :: Maybe Text  
  , _CardObject_loyalty       :: Maybe Natural 
  , _CardObject_multiverseid  :: Maybe Natural
   -- ^ The multiverseid of the card on Wizard's Gatherer web page.
   -- Cards from sets that do not exist on Gatherer will NOT have a multiverseid.
   -- Sets not on Gatherer are: ATH, ITP, DKM, RQS, DPA and all sets with a 4 letter code that starts with a lowercase 'p'.
    
  , _CardObject_variations    :: Maybe [Natural] 
--  , _CardObject_imageName     :: Maybe Text 
  , _CardObject_watermark     :: Maybe Text 
  , _CardObject_border        :: Maybe Text 
  , _CardObject_timeshifted   :: Maybe Bool -- IsCardTimeShifted
  , _CardObject_hand          :: Maybe Integer  -- ^ Vanguard only 
  , _CardObject_life          :: Maybe Integer -- ^ Vanguard only 
  , _CardObject_reserved      :: Maybe Bool -- IsCardReserved 
  , _CardObject_releaseDate   :: Maybe Text -- ^ Promo only 
  , _CardObject_starter       :: Maybe Bool -- IsCardStarter 
  , _CardObject_mciNumber     :: Maybe Text
    -- ^ used by `MagicCards.info`, almost always identical to '_CardObject_number'.
    -- 
  , _CardObject_rulings       :: Maybe [CardRulingObject] 
  , _CardObject_foreignNames  :: Maybe [CardForeignPrintingObject] 
  , _CardObject_printings     :: Maybe [Text]
    -- ^ e.g. ["ICE", "CHR"]
    -- The sets that this card was printed in, expressed as an array of set codes.
  , _CardObject_originalText  :: Maybe Text 
  , _CardObject_originalType  :: Maybe Text
  , _CardObject_legalities    :: Maybe [CardFormatLegalityObject]
  , _CardObject_source        :: Maybe Text 
  }
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable )
instance FromJSON CardObject where
  parseJSON = parseJSON_TypePrefix

----------------------------------------

{-| 

-}
data CardForeignPrintingObject = CardForeignPrintingObject 
  { _CardForeignPrintingObject_language     :: Text 
  , _CardForeignPrintingObject_name         :: Text 
  , _CardForeignPrintingObject_multiverseid :: Maybe Natural 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable )
instance FromJSON CardForeignPrintingObject where
  parseJSON = parseJSON_TypePrefix
  
{-| 

-}
data CardFormatLegalityObject = CardFormatLegalityObject 
  { _CardFormatLegalityObject_format   :: Text 
  , _CardFormatLegalityObject_legality :: Text 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable )
instance FromJSON CardFormatLegalityObject where
  parseJSON = parseJSON_TypePrefix

{-| 

-}
data CardRulingObject = CardRulingObject 
  { _CardRulingObject_date :: Text -- NOTE time-UTCTime / time-Day / etc have no Generic instance
  , _CardRulingObject_text :: Text 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable )
instance FromJSON CardRulingObject where
  parseJSON = parseJSON_TypePrefix

----------------------------------------
