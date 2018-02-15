{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
 
{-| The core types. 

This module mostly defines types 
(i.e. @data@, @newtype@, @type@, @class@, @instance@) 
and whatever values are necessary for instances.

-}
module Cards.Frontend.Types where
import Cards.Frontend.Extra

--import qualified Data.Text as T
--import Data.Text (Text)
--import Data.Text (pack, unpack)

--import Text.Read (readMaybe)
-- import Data.Monoid
-- import System.Environment

import Prelude.Spiros hiding (Text)
import GHC.Exts (IsList)

----------------------------------------

type RawQuery = Text
--type Query = ValidQuery

--newtype RawQuery = RawQuery { fromRawQuery :: Text } deriving ()

newtype ValidQuery = ValidQuery { fromValidQuery :: Text } deriving ()

data QueryOptions = QueryOptions
 { _queryLanguage :: QueryLanguage
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

instance Default QueryOptions where
  def = QueryOptions def 

{-| ParseQueryLike... -}
data QueryLanguage
  = ParseQueryLikeMCI -- ^ @magiccards.info@'s sytax
  | ParseQueryLikeProlog -- ^ 
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Ix,Generic,NFData,Hashable)

instance Default QueryLanguage where def = ParseQueryLikeMCI

----------------------------------------

newtype CardDatabase = CardDatabase { fromCardDatabase :: [Card] } deriving ()

data Card = Card 
 { _cardName :: Text 
 , _cardText :: Text
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

----------------------------------------

newtype Results = Results { fromResults :: [Result] }
 deriving (IsList)

newtype Result = Result { fromResult :: Card }
 deriving ()

data ResultsOptions = ResultsOptions
 { _resultsFormat :: ResultsFormat 
 , _resultsOrder  :: ResultsOrder
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

instance Default ResultsOptions where
  def = ResultsOptions def def

{-| DisplayResultsAs... -}
data ResultsFormat
  = DisplayResultsAsText
  | DisplayResultsAsImages
  | DisplayResultsAsHTML
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Ix,Generic,NFData,Hashable)

instance Default ResultsFormat where def = DisplayResultsAsText

{-| SortResultsBy... -}
data ResultsOrder
  = SortResultsByName
  | SortResultsByEdition
  | SortResultsByColor
  | SortResultsByType
  | SortResultsByCmc
  | SortResultsByRarity
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Ix,Generic,NFData,Hashable)

instance Default ResultsOrder where def = SortResultsByName

----------------------------------------
{-

deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

deriving (Show,Read,Eq,Ord,Enum,Bounded,Ix,Generic,NFData,Hashable)

-}