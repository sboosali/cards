{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.AllSets.Fold where

import MTGJSON.Extra --hiding (Text)
import MTGJSON.AllSets.Schema
-- import MTGJSON.AllSets.Types
-- import MTGJSON.Types

-- import MTGJSON.Known.Types
-- import MTGJSON.Known.Lens
-- import MTGJSON.Known.Newtypes

-- import Control.Lens hiding (Fold, (<&>))

-- import "scientific" Data.Scientific

import qualified "foldl" Control.Foldl as L
import           "foldl" Control.Foldl (Fold(..))
-- --import qualified "foldl" Control.Foldl.Text as LT

-- import qualified Data.Text as T
-- import           Data.Text (Text)

-- import qualified Data.Map as Map
-- import           Data.Map (Map)

-- import qualified Data.Set as Set
-- import           Data.Set (Set)

-- import Data.Ratio (Ratio, (%))

----------------------------------------
-- Summarizer

{-
summarizeCards
  :: ( Foldable f
     )
  => f CardObject
  -> CardSummary 
summarizeCards = L.fold cardFold

----------------------------------------
-- Folder

type CardFold = Fold CardObject CardSummary

cardFold :: Fold CardObject CardSummary
cardFold = _ --CardSummary
-}

----------------------------------------
-- 

data CardSummary = CardSummary
  { _CardSummary_id            :: Text 
  , _CardSummary_name          :: Text 
  , _CardSummary_layout        :: Maybe Text 
  , _CardSummary_names         :: Maybe [Text] 
  , _CardSummary_manaCost      :: Maybe Text 
  , _CardSummary_cmc           :: Scientific 
  , _CardSummary_colors        :: Maybe [Text] 
  , _CardSummary_colorIdentity :: Maybe [Text] 
  , _CardSummary_type          :: Text 
  , _CardSummary_supertypes    :: Maybe [Text] 
  , _CardSummary_types         :: Maybe [Text] 
  , _CardSummary_subtypes      :: Maybe [Text] 
  , _CardSummary_rarity        :: Text 
  , _CardSummary_text          :: Maybe Text 
  , _CardSummary_flavor        :: Maybe Text 
  , _CardSummary_artist        :: Text
  , _CardSummary_number        :: Maybe Text 
  , _CardSummary_power         :: Maybe Text 
  , _CardSummary_toughness     :: Maybe Text  
  , _CardSummary_loyalty       :: Maybe Natural 
  , _CardSummary_multiverseid  :: Maybe Natural
  , _CardSummary_variations    :: Maybe [Natural] 
  , _CardSummary_imageName     :: Maybe Text 
  , _CardSummary_watermark     :: Maybe Text 
  , _CardSummary_border        :: Maybe Text 
  , _CardSummary_timeshifted   :: Maybe Bool 
  , _CardSummary_hand          :: Maybe Integer  
  , _CardSummary_life          :: Maybe Integer 
  , _CardSummary_reserved      :: Maybe Bool 
  , _CardSummary_releaseDate   :: Maybe Text 
  , _CardSummary_starter       :: Maybe Bool 
  , _CardSummary_mciNumber     :: Maybe Text  
  , _CardSummary_rulings       :: Maybe [CardRulingObject] 
  , _CardSummary_foreignNames  :: Maybe [CardForeignPrintingObject] 
  , _CardSummary_printings     :: Maybe [Text]
  , _CardSummary_originalText  :: Maybe Text 
  , _CardSummary_originalType  :: Maybe Text
  , _CardSummary_legalities    :: Maybe [CardFormatLegalityObject]
  , _CardSummary_source        :: Maybe Text 
  } deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable )

----------------------------------------
-- 

data CardFolds = CardFolds
  { _CardFolds_id            :: Text 
  , _CardFolds_name          :: Text 
  , _CardFolds_layout        :: Maybe Text 
  , _CardFolds_names         :: Maybe [Text] 
  , _CardFolds_manaCost      :: Maybe Text 
  , _CardFolds_cmc           :: Scientific 
  , _CardFolds_colors        :: Maybe [Text] 
  , _CardFolds_colorIdentity :: Maybe [Text] 
  , _CardFolds_type          :: Text 
  , _CardFolds_supertypes    :: Maybe [Text] 
  , _CardFolds_types         :: Maybe [Text] 
  , _CardFolds_subtypes      :: Maybe [Text] 
  , _CardFolds_rarity        :: Text 
  , _CardFolds_text          :: Maybe Text 
  , _CardFolds_flavor        :: Maybe Text 
  , _CardFolds_artist        :: Text
  , _CardFolds_number        :: Maybe Text 
  , _CardFolds_power         :: Maybe Text 
  , _CardFolds_toughness     :: Maybe Text  
  , _CardFolds_loyalty       :: Maybe Natural 
  , _CardFolds_multiverseid  :: Maybe Natural
  , _CardFolds_variations    :: Maybe [Natural] 
  , _CardFolds_imageName     :: Maybe Text 
  , _CardFolds_watermark     :: Maybe Text 
  , _CardFolds_border        :: Maybe Text 
  , _CardFolds_timeshifted   :: Maybe Bool 
  , _CardFolds_hand          :: Maybe Integer  
  , _CardFolds_life          :: Maybe Integer 
  , _CardFolds_reserved      :: Maybe Bool 
  , _CardFolds_releaseDate   :: Maybe Text 
  , _CardFolds_starter       :: Maybe Bool 
  , _CardFolds_mciNumber     :: Maybe Text  
  , _CardFolds_rulings       :: Maybe [CardRulingObject] 
  , _CardFolds_foreignNames  :: Maybe [CardForeignPrintingObject] 
  , _CardFolds_printings     :: Maybe [Text]
  , _CardFolds_originalText  :: Maybe Text 
  , _CardFolds_originalType  :: Maybe Text
  , _CardFolds_legalities    :: Maybe [CardFormatLegalityObject]
  , _CardFolds_source        :: Maybe Text 
  } deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable )


----------------------------------------  

----------------------------------------  
{-


-}
