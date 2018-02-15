{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

--import Prelude.Spiros hiding (Text)
--import GHC.Exts (IsList(..))

----------------------------------------

data ResultsPage
 = InitialResultsPage
 | FailedResultsPage     SearchError
 | SuccessfulResultsPage Results
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

----------------------------------------

newtype SearchError = SearchError
  { fromSearchError :: Text }
  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable) --,IsString)

instance IsString SearchError where
  fromString = fromString > SearchError
  --toString   = fromSearchError

----------------------------------------

type RawQuery = Text
--type Query = ValidQuery

--newtype RawQuery = RawQuery { fromRawQuery :: Text } deriving ()

newtype ValidQuery = ValidQuery { fromValidQuery :: Text }
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

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

data CardDatabase = CardDatabase
  { fromCardDatabase :: [Card]
  --TODO , hashedCardDatabase :: Int -- for caching
  } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

data Card = Card 
 { _cardName :: Text 
 , _cardText :: Text
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

----------------------------------------

newtype Results = Results { fromResults :: [Result] }
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

instance IsList Results where
  type Item Results = Result
  fromList = Results
  toList = fromResults

newtype Result = Result
 { fromResult :: Card }
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

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
