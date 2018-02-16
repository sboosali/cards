
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

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

--import Data.Time (NominalDiffTime)

--import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (nonEmpty)

----------------------------------------

data ResultsPage
 = InitialResultsPage
 | FailedResultsPage     SearchError
 | SuccessfulResultsPage Results
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

instance Default ResultsPage where
  def = InitialResultsPage

----------------------------------------

data InterfaceConfig = InterfaceConfig
 { _cSearchOptions  :: SearchOptions
 , _cQueryOptions   :: QueryOptions
 , _cResultsOptions :: ResultsOptions
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

instance Default InterfaceConfig where
  def = defaultInterfaceConfig

defaultInterfaceConfig :: InterfaceConfig
defaultInterfaceConfig = InterfaceConfig def def def

----------------------------------------

data SearchOptions = SearchOptions
 { _requireSubmitOrEnter              :: Bool -- ^ whether the "submit" button must be clicked.
 , _debouncingDelayInMilliseconds     :: Double
   -- NominalDiffTime
 , _minimumQueryLengthForLiveSearch   :: Natural
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

instance Default SearchOptions where
  def = defaultSearchOptions

{-|

* '_requireSubmitOrEnter': a.k.a. @True@ disables "live search". @False@ by default.

-}
defaultSearchOptions :: SearchOptions
defaultSearchOptions = SearchOptions
 { _requireSubmitOrEnter              = False
 , _debouncingDelayInMilliseconds     = 500
 , _minimumQueryLengthForLiveSearch   = 3
 
 }

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
  = ParseQueryLikeMCI    -- ^ @magiccards.info@'s sytax
  | ParseQueryLikeProlog -- ^
  | ParseQueryAsFreeText -- ^
  | ParseQueryLikeSQL    -- ^
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
 { fromResult :: Card
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

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

----------------------------------------

{-| SortResultsBy... -}
data ResultsOrder = ResultsOrder
  { fromResultsOrder :: (NonEmpty SortResultsBy) -- ^ a @Set@
  }
  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)
  --NOTE we don't use Set itself because (1) we want it to be non-empty and (2) Sets aren't Generic

instance Default ResultsOrder where
  def = defaultResultsOrder

instance IsList ResultsOrder where
  type Item ResultsOrder = SortResultsBy
  fromList = nonEmpty > maybe [def] id > ResultsOrder
  toList   = fromResultsOrder > toList

defaultResultsOrder :: ResultsOrder
defaultResultsOrder =
  [ SortResultsByEdition
  , SortResultsByName
  ]

data SortResultsBy
  = SortResultsByName
  | SortResultsByEdition
  | SortResultsByColor
  | SortResultsByType
  | SortResultsByCmc
  | SortResultsByRarity
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Ix,Generic,NFData,Hashable)

instance Default SortResultsBy where def = SortResultsByName
      
----------------------------------------
{-

deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

deriving (Show,Read,Eq,Ord,Enum,Bounded,Ix,Generic,NFData,Hashable)

-}
