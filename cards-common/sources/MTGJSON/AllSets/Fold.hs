{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.AllSets.Fold where

import MTGJSON.Extra --hiding (Text)
import MTGJSON.AllSets.Set
import MTGJSON.AllSets.Card

import MTGJSON.AllSets.Enums
import MTGJSON.AllSets.Booster

-- import MTGJSON.AllSets.Types
-- import MTGJSON.Types

-- import MTGJSON.Known.Types
-- import MTGJSON.Known.Lens
-- import MTGJSON.Known.Newtypes

-- import Control.Lens hiding (Fold, (<&>))

-- import "scientific" Data.Scientific

--import Enumerate

import                   Control.Foldl.Summary
--import qualified "foldl" Control.Foldl as L
import           "foldl" Control.Foldl (Fold(..))
-- --import qualified "foldl" Control.Foldl.Text as LT

import "thyme" Data.Thyme.Calendar

-- import qualified Data.Text as T
-- import           Data.Text (Text)

-- import qualified Data.Map as Map
-- import           Data.Map (Map)

-- import qualified Data.Set as Set
-- import           Data.Set (Set)

-- import Data.Ratio (Ratio, (%))

--import GHC.Generics (Generic1)

----------------------------------------
-- Summarizer

----------------------------------------
-- Folder
type Summary = ()

type CardFold = Fold CardSchema CardSummary

type CardFolds = CardP Fold

----------------------------------------
-- Sets

data EditionSummary = EditionSummary

{-
summarizeCards
  :: ( Foldable f
     )
  => f CardSchema
  -> CardSummary 
summarizeCards = L.fold cardFold

editionFold :: Fold Edition EditionSummary
editionFold = _ --EditionSummary
-}

----------------------------------------
--

data EditionP p = EditionP

  { _EditionP_name               :: !(p EditionName
                                    )
    -- ^ "Nemesis",
    -- The name of the set

  , _EditionP_codes              :: !(p EditionCodes
                                    )
    -- ^ 

  , _EditionP_block              :: !(p BlockName
                                    )
    -- ^ e.g. "Masques"
    -- The block this set is in. 

  , _EditionP_type               :: !(p EditionType
                                    )
    -- ^ e.g. "expansion"
    -- The type of set.
    -- One of: "core", "expansion", "reprint", "box", "un", "from the vault", "premium deck", "duel deck", "starter", "commander", "planechase", "archenemy","promo", "vanguard", "masters", "conspiracy", "masterpiece". 

  , _EditionP_border             :: !(p Border
                                    )
    -- ^ "black",
    -- The type of border on the cards,
    -- either "white", "black" or "silver". 

  , _EditionP_booster            :: !(p Booster
                                    )
    -- ^ e.g. @[ "rare", ... ]@ 
    -- Booster contents for this set

  , _EditionP_releaseDate        :: !(p Day
                                    )
    -- ^ "2000-02-14"
    -- When the set was released (YYYY-MM-DD). 
    -- (For promo sets, the date the first card was released).

  , _EditionP_onlineOnly         :: !(p WhetherOffline
                                    )
    -- ^ if the set was only released online (i.e. not in paper). 

  {-
  , _EditionP_cards              :: [card]   -- ^ [ {}, {}, {}, ... ]    -- ^ 
  -}
  
  } deriving (Generic)

-- instance NFData     (EditionP p)
-- instance Hashable   (EditionP p)

----------------------------------------

data EditionCodesP p = EditionCodesP

 { _EditionP_primaryCode        :: !(p (Text))
   -- ^ TODO e.g. "NMS"
   -- The set's abbreviated code

 , _EditionP_gathererCode       :: !(p (Text))
   -- ^ e.g. "NE"
   -- The code that Gatherer uses for the set.
   -- Normally identical to '_EditionP_primaryCode'. 

 , _EditionP_oldCode            :: !(p (Text))
   -- ^ e.g. "NEM"
   -- The (deprecated) old-style code, used by some Magic software.
   -- Normally identical to ' _EditionP_'gathererCode' (or '_EditionP_primaryCode')

 , _EditionP_magicCardsInfoCode :: !(p (Maybe Text))
   -- ^ e.g. "ne"
   -- The code that magiccards.info uses for the set.
   -- @Nothing@ if absent from @magiccards.info@. 
   -- Normally identical to ' _EditionP_'gathererCode' (or '_EditionP_primaryCode')
 
 } deriving (Generic)

-- instance NFData     (EditionCodesP p)
-- instance Hashable   (EditionCodesP p)

  
----------------------------------------
-- Cards

data CardSummary = CardSummary

-- cardFold :: Fold CardSchema CardSummary
-- cardFold = _ --CardSummary

----------------------------------------
--

{-|

Naming: @CardP@ means "card profunctor". 

-}
data CardP (p :: * -> * -> *) = CardP
  { _CardFolds_id            :: !(p Text
                                    TextualSummary)

  , _CardFolds_name          :: !(p Text
                                    TextualSummary)

  , _CardFolds_layout        :: !(p (Maybe Text)
                                    TextualSummary)

  , _CardFolds_names         :: !(p (Maybe [Text])
                                    Summary)

  , _CardFolds_manaCost      :: !(p (Maybe Text)
                                    TextualSummary)

  , _CardFolds_cmc           :: !(p Scientific
                                    (NumericSummary Scientific))

  , _CardFolds_colors        :: !(p (Maybe [Text])
                                    Summary)

  , _CardFolds_colorIdentity :: !(p (Maybe [Text])
                                    Summary)

  , _CardFolds_type          :: !(p Text
                                    TextualSummary)

  , _CardFolds_supertypes    :: !(p (Maybe [Text])
                                    Summary)

  , _CardFolds_types         :: !(p (Maybe [Text])
                                    Summary)

  , _CardFolds_subtypes      :: !(p (Maybe [Text])
                                    Summary)

  , _CardFolds_rarity        :: !(p Text
                                    TextualSummary)

  , _CardFolds_text          :: !(p (Maybe Text)
                                    TextualSummary)

  , _CardFolds_flavor        :: !(p (Maybe Text)
                                    TextualSummary)

  , _CardFolds_artist        :: !(p Text
                                    TextualSummary)

  , _CardFolds_number        :: !(p (Maybe Text)
                                    TextualSummary)

  , _CardFolds_power         :: !(p (Maybe Text)
                                    TextualSummary)

  , _CardFolds_toughness     :: !(p (Maybe Text )
                                    TextualSummary)

  , _CardFolds_loyalty       :: !(p (Maybe Natural)
                                   (NumericSummary Natural))

  , _CardFolds_multiverseid  :: !(p (Maybe Natural)
                                    (NumericSummary Natural))

  , _CardFolds_variations    :: !(p (Maybe [Natural])
                                    Summary)

  , _CardFolds_imageName     :: !(p (Maybe Text)
                                    TextualSummary)

  , _CardFolds_watermark     :: !(p (Maybe Text)
                                    TextualSummary)

  , _CardFolds_border        :: !(p (Maybe Text)
                                    TextualSummary)

  , _CardFolds_timeshifted   :: !(p (Maybe Bool)
                                    (EnumSummary Bool))

  , _CardFolds_hand          :: !(p (Maybe Integer )
                                    (NumericSummary Integer))

  , _CardFolds_life          :: !(p (Maybe Integer)
                                    (NumericSummary Integer))

  , _CardFolds_reserved      :: !(p (Maybe Bool)
                                    (EnumSummary Bool))

  , _CardFolds_releaseDate   :: !(p (Maybe Text)
                                    Summary)

  , _CardFolds_starter       :: !(p (Maybe Bool)
                                    (EnumSummary Bool))

  , _CardFolds_mciNumber     :: !(p (Maybe Text)
                                    TextualSummary)

  , _CardFolds_rulings       :: !(p [Ruling]
                                    Summary)

  , _CardFolds_foreignNames  :: !(p [ForeignPrinting]
                                    Summary)

  , _CardFolds_printings     :: !(p (Maybe [Text])
                                    Summary)

  , _CardFolds_originalText  :: !(p (Maybe Text)
                                    TextualSummary)

  , _CardFolds_originalType  :: !(p (Maybe Text)
                                    TextualSummary)

  , _CardFolds_legalities    :: !(p [FormatLegality]
                                    Summary)

  , _CardFolds_source        :: !(p (Maybe Text)
                                    TextualSummary)
                                
  } -- deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

----------------------------------------  

----------------------------------------  
{-


-}
