{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase, DeriveAnyClass #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables #-}
-- {-# LANGUAGE TypeApplications #-} -- NOTE needs GHC822

{-|

-}
module MTGJSON.AllSets.Schema where 
--import Cards.Common.Extra -- hiding (ByteString)  
import Cards.MTGJSON.Aeson

import Data.Aeson (eitherDecode) 
import Data.Aeson.Types
 (FromJSON(..),Options(..), SumEncoding(..), defaultOptions, genericParseJSON) 

-- import Data.Generics.Product 
-- import qualified Control.Lens as L 

import qualified Data.Binary as Binary 
import Data.Binary (Binary)

-- import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (ByteString) 

import Prelude.Spiros

----------------------------------------

-- test theName = L.set (field @"_SetObject_name") theName 

persistSetsObject :: FilePath -> SetsObject -> IO ()
persistSetsObject s o = Binary.encodeFile s o

restoreSetsObject :: FilePath -> IO (SetsObject)
restoreSetsObject s = Binary.decodeFile s

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
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,FromJSON,Binary) -- NOTE containers-Map is not Hashable 

{-| 

-}
data SetsArray = SetsArray [SetObject]
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,FromJSON,Binary) -- NOTE containers-Map is not Hashable 

----------------------------------------
-- SetObject

{-| 

-}
data SetObject = SetObject 
  { _SetObject_name               :: Text  -- ^ "Nemesis",       // The name of the set
  , _SetObject_code               :: Text  -- ^ "NMS",           // The set's abbreviated code
  -- , _SetObject_gathererCode       :: Maybe Text  -- ^ "NE",            // The code that Gatherer uses for the set. Only present if different than 'code'
  -- , _SetObject_oldCode            :: Maybe Text  -- ^ "NEM",           // An old style code used by some Magic software. Only present if different than 'gathererCode' and 'code'
  -- , _SetObject_magicCardsInfoCode :: Maybe Text  -- ^ "ne",            // The code that magiccards.info uses for the set. Only present if magiccards.info has this set
  -- , _SetObject_releaseDate        :: Maybe Text  -- ^ "2000-02-14"     // When the set was released (YYYY-MM-DD). For promo sets, the date the first card was released.
  -- , _SetObject_border             :: Text  -- ^ "black",         // The type of border on the cards, either "white", "black" or "silver"
  -- , _SetObject_type               :: Text  -- ^ "expansion",     // Type of set. One of: "core", "expansion", "reprint", "box", "un", "from the vault", "premium deck", "duel deck", "starter", "commander", "planechase", "archenemy","promo", "vanguard", "masters", "conspiracy", "masterpiece"
  -- , _SetObject_block              :: Maybe Text  -- ^ "Masques",       // The block this set is in,
  -- , _SetObject_onlineOnly         :: Maybe Bool  -- ^ false,           // Present and set to true if the set was only released online
  -- , _SetObject_booster            :: Maybe MagicBoosterObject  -- ^ [ "rare", ... ], // Booster contents for this set, see below for details
  , _SetObject_cards              :: [CardObject]   -- ^ [ {}, {}, {}, ... ]  
  } deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable, Binary )
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
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable, Binary )
instance FromJSON MagicBoosterSlotObject where
  parseJSON = genericParseJSON defaultOptions
    { sumEncoding = UntaggedValue
    }

----------------------------------------
-- CardObject

data CardObject = CardObject 
  { _CardObject_id            :: Text 
--  , _CardObject_layout        :: Text 
  , _CardObject_name          :: Text 
  , _CardObject_names         :: Maybe [Text] 
  , _CardObject_manaCost      :: Maybe Text 
  , _CardObject_cmc           :: Natural 
  , _CardObject_colors        :: Maybe [Text] 
  , _CardObject_colorIdentity :: Maybe [Text] 
  , _CardObject_type          :: Text 
  , _CardObject_supertypes    :: Maybe [Text] 
  , _CardObject_types         :: Maybe [Text] -- ^ Un-cards can have no type 
  , _CardObject_subtypes      :: Maybe [Text] 
  , _CardObject_rarity        :: Text 
  , _CardObject_text          :: Maybe Text 
  , _CardObject_flavor        :: Maybe Text 
  , _CardObject_artist        :: Text
  , _CardObject_number        :: Maybe Text
  , _CardObject_power         :: Maybe Text -- ^ Un-cards can have non-integer power/toughness 
  , _CardObject_toughness     :: Maybe Text  
  , _CardObject_loyalty       :: Maybe Natural 
  , _CardObject_multiverseid  :: Maybe Natural
  , _CardObject_variations    :: Maybe [Natural] 
  , _CardObject_imageName     :: Maybe Text 
  , _CardObject_watermark     :: Maybe Text 
  , _CardObject_border        :: Maybe Text 
  , _CardObject_timeshifted   :: Maybe Bool -- IsCardTimeShifted
  , _CardObject_hand          :: Maybe Integer  -- ^ Vanguard only 
  , _CardObject_life          :: Maybe Integer -- ^ Vanguard only 
  , _CardObject_reserved      :: Maybe Bool -- IsCardReserved 
  , _CardObject_releaseDate   :: Maybe Text -- ^ Promo only 
  , _CardObject_starter       :: Maybe Bool -- IsCardStarter 
  , _CardObject_mciNumber     :: Maybe Text  -- ^ used by `MagicCards.info`, almost always identical to '_CardObject_number' 
  , _CardObject_rulings       :: Maybe [CardRulingObject] 
  , _CardObject_foreignNames  :: Maybe [CardForeignPrintingObject] 
--  , _CardObject_printings     :: [Text]  
  , _CardObject_originalText  :: Maybe Text 
  , _CardObject_originalType  :: Maybe Text
  , _CardObject_legalities    :: Maybe [CardFormatLegalityObject]
  , _CardObject_source        :: Maybe Text 
  }
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable, Binary )
instance FromJSON CardObject where
  parseJSON = parseJSON_TypePrefix

----------------------------------------

{-| 

-}
data CardForeignPrintingObject = CardForeignPrintingObject 
  { _CardForeignPrintingObject_language     :: Text 
  , _CardForeignPrintingObject_name         :: Text 
  , _CardForeignPrintingObject_multiverseid :: Maybe Natural 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable, Binary )
instance FromJSON CardForeignPrintingObject where
  parseJSON = parseJSON_TypePrefix

{-| 

-}
data CardRulingObject = CardRulingObject 
  { _CardRulingObject_date :: Text -- NOTE time-UTCTime / time-Day / etc have no Generic instance
  , _CardRulingObject_text :: Text 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable, Binary )
instance FromJSON CardRulingObject where
  parseJSON = parseJSON_TypePrefix
  
{-| 

-}
data CardFormatLegalityObject = CardFormatLegalityObject 
  { _CardFormatLegalityObject_format   :: Text 
  , _CardFormatLegalityObject_legality :: Text 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable, Binary )
instance FromJSON CardFormatLegalityObject where
  parseJSON = parseJSON_TypePrefix

----------------------------------------
