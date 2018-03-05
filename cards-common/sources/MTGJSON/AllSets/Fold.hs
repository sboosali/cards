{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.AllSets.Fold where

import MTGJSON.Extra --hiding (Text)
import MTGJSON.AllSets.Object -- Schema
-- import MTGJSON.AllSets.Types
-- import MTGJSON.Types

-- import MTGJSON.Known.Types
-- import MTGJSON.Known.Lens
-- import MTGJSON.Known.Newtypes

-- import Control.Lens hiding (Fold, (<&>))

-- import "scientific" Data.Scientific

import Enumerate

import                   Control.Foldl.Summary
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

{--}

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

type CardFolds = CardP Fold

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

  , _CardFolds_rulings       :: !(p [CardRulingObject]
                                    Summary)

  , _CardFolds_foreignNames  :: !(p [CardForeignPrintingObject]
                                    Summary)

  , _CardFolds_printings     :: !(p (Maybe [Text])
                                    Summary)

  , _CardFolds_originalText  :: !(p (Maybe Text)
                                    TextualSummary)

  , _CardFolds_originalType  :: !(p (Maybe Text)
                                    TextualSummary)

  , _CardFolds_legalities    :: !(p [CardFormatLegalityObject]
                                    Summary)

  , _CardFolds_source        :: !(p (Maybe Text)
                                    TextualSummary)
                                
  } deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

----------------------------------------  

----------------------------------------  
{-


-}
