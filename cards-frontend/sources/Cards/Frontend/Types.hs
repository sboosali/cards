
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE DeriveAnyClass #-}
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| The core types. 

This module mostly defines types 
(i.e. @data@, @newtype@, @type@, @class@, @instance@) 
and whatever values are necessary for instances.

-}
module Cards.Frontend.Types where
import Cards.Frontend.Extra

import Reflex
import Reflex.Dom

--import qualified Data.Text as T
--import Data.Text (Text)
--import Data.Text (pack, unpack)

--import Text.Read (readMaybe)
-- import Data.Monoid
-- import System.Environment

--import Prelude.Spiros hiding (Text)
--import GHC.Exts (IsList(..))

--import Data.Time (NominalDiffTime)

import qualified Data.Set           as Set
import qualified Data.List.NonEmpty as NonEmpty


----------------------------------------

--TODO
type Runner = Frontend -> IO_  -- JSM ()

--TODO mv
data Frontend = Frontend
 { _widgetHead :: SomeWidget ()
 , _widgetBody :: JAVASCRIPT_RUNNER -> SomeWidget ()
 
-- , _widgetBody :: SomeWidget (())
-- , _widgetBody :: SomeWidget (JAVASCRIPT_RUNNER -> ())
-- , _widgetBody :: JAVASCRIPT_RUNNER -> SomeWidget ()

 -- , wBody :: (MonadJSM IO) => SomeWidget_
-- , wCSS  :: Either Text FilePath -- ^ `inline` or `link`ed
 }

----------------------------------------

--TODO store abstract constraints versus store concrete datatypes

-- | existentially-quantified widget (alias).
type XWidget a = forall x. Widget x a

-- | existentially-quantified widget (alias).
type XWidget_  = XWidget ()

-- | existentially-quantified widget (datatype).  
data SomeWidget a
  = SomeWidget (XWidget a)

-- | existentially-quantified widget (datatype).  
data SomeWidget_ 
  = SomeWidget_ XWidget_

-- data SomeJSaddleWidget a
--   = SomeJSaddleWidget (forall t m. MonadJSaddleWidget t m => m a)
  
-- data SomeJSaddleWidget a
--   = SomeJSaddleWidget (forall t m. MonadJSaddleWidget t m => m a)
  
-- data SomeJSaddleWidget a
--   = SomeJSaddleWidget (forall x.                  Widget x a)

-- data SomeJSaddleWidget a
--   = SomeJSaddleWidget (forall x. (MonadJSM IO) => Widget x a)


----------------------------------------

data ResultsPage
 = InitialResultsPage
 | FailedResultsPage     SearchError
 | SuccessfulResultsPage Results
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

instance Default ResultsPage where
  def = InitialResultsPage
  
----------------------------------------

data ResultsPageConfig t = ResultsPageConfig  
 { dInterfaceConfig :: Dynamic t InterfaceConfig
 , eSubmitButton    :: Event   t ()
 , oSearchBar       :: TextInput t
 }

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

pSearchOptions :: Proxy SearchOptions
pSearchOptions = Proxy

{-|

* '_requireSubmitOrEnter': a.k.a. @True@ disables "live search". @False@ by default.

-}
defaultSearchOptions :: SearchOptions
defaultSearchOptions = SearchOptions
 { _requireSubmitOrEnter              = False
 , _debouncingDelayInMilliseconds     = 500
 , _minimumQueryLengthForLiveSearch   = 3
 }

fasterSearchOptions :: SearchOptions
fasterSearchOptions = SearchOptions
 { _requireSubmitOrEnter              = False
 , _debouncingDelayInMilliseconds     = 100
 , _minimumQueryLengthForLiveSearch   = 1
 }

manualSearchOptions :: SearchOptions
manualSearchOptions = SearchOptions
 { _requireSubmitOrEnter              = False
 , _debouncingDelayInMilliseconds     = 0
 , _minimumQueryLengthForLiveSearch   = 0
 }

----------------------------------------

type RawQuery = Text
--type Query = ValidQuery

--newtype RawQuery = RawQuery { fromRawQuery :: Text } deriving ()

newtype ValidQuery = ValidQuery { fromValidQuery :: Text }
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

----------------------------------------

data QueryOptions = QueryOptions
 { _queryLanguage :: QueryLanguage
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

instance Default QueryOptions where
  def = QueryOptions def 

pQueryOptions :: Proxy QueryOptions
pQueryOptions = Proxy

{-| ParseQueryLike... -}
data QueryLanguage
  = ParseQueryLikeMCI    -- ^ @magiccards.info@'s sytax
  | ParseQueryAsFreeText -- ^
 --TODO  | ParseQueryLikeProlog -- ^
 --TODO  | ParseQueryLikeSQL    -- ^
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Ix,Generic,NFData,Hashable)

instance Default QueryLanguage where def = ParseQueryLikeMCI

pQueryLanguage :: Proxy QueryLanguage
pQueryLanguage = Proxy

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

----------------------------------------

data ResultsOptions = ResultsOptions
 { _resultsFormat :: ResultsFormat 
 , _resultsOrder  :: ResultsOrder
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

instance Default ResultsOptions where
  def = ResultsOptions def def
  
pResultsOptions :: Proxy ResultsOptions
pResultsOptions = Proxy

----------------------------------------

{-| DisplayResultsAs... -}
data ResultsFormat
  = DisplayResultsAsTextOnly
  | DisplayResultsAsImagesOnly
  | DisplayResultsAsRichTextWithCroppedImagesViaHTML
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Ix,Generic,NFData,Hashable)

instance Default ResultsFormat where def = DisplayResultsAsTextOnly

pResultsFormat :: Proxy ResultsFormat
pResultsFormat = Proxy

----------------------------------------

{-| SortResultsBy... -}
data ResultsOrder = ResultsOrder
  { fromResultsOrder :: (NonEmpty SortResultsBy) -- ^ a non-empty @Set@
  }
  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)
  --NOTE we don't use Set itself because (1) we want it to be non-empty and (2) Sets aren't Generic

instance Default ResultsOrder where
  def = defaultResultsOrder

instance IsList ResultsOrder where
  type Item ResultsOrder = SortResultsBy
  fromList = toNonEmptyDefaulting > ResultsOrder
  toList   = fromResultsOrder > toList

defaultResultsOrder :: ResultsOrder
defaultResultsOrder =
  [ SortResultsByEdition
  , SortResultsByName
  ]

fromResultsOrder' :: ResultsOrder -> Set SortResultsBy
fromResultsOrder'
  = fromResultsOrder
  > NonEmpty.toList
  > Set.fromList

toResultsOrder :: Set SortResultsBy -> ResultsOrder
toResultsOrder = toNonEmptyDefaulting > ResultsOrder

singularResultsOrder :: SortResultsBy -> ResultsOrder 
singularResultsOrder = (:|[]) > ResultsOrder

data SortResultsBy
  = SortResultsByName
  | SortResultsByEdition
  | SortResultsByColor
  | SortResultsByType
  | SortResultsByCmc
  | SortResultsByRarity
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Ix,Generic,NFData,Hashable)

instance Default SortResultsBy where def = SortResultsByName

pSortResultsBy :: Proxy SortResultsBy
pSortResultsBy = Proxy

----------------------------------------

newtype SearchError = SearchError
  { fromSearchError :: Text }
  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable) --,IsString)

instance IsString SearchError where
  fromString = fromString > SearchError
  --toString   = fromSearchError

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

pBool :: Proxy Bool
pBool = Proxy

----------------------------------------

-- | one of the known javacsript runners (i.e. representatios the @jsaddle-*@ family of packages). 
data JAVASCRIPT_RUNNER
  = BROWSER --TODO???
  | NODEJS --TODO???
  | JSADDLEWARP
  | JSADDLEWEBSOCKETS
  | WEBKITGTK
  | WKWEBVIEW
  deriving (Show)
--introspect on the runner.

displayJavascriptRunner :: JAVASCRIPT_RUNNER -> Text
displayJavascriptRunner = \case
  BROWSER           -> "browser"
  NODEJS            -> "node.js"
  JSADDLEWARP       -> "JSaddle-Warp" -- "jsaddle-warp"
  JSADDLEWEBSOCKETS -> "JSaddle-WebSockets" 
  WEBKITGTK         -> "WebKitGTK"    -- "webkit-gtk"
  WKWEBVIEW         -> "WKWebView"

----------------------------------------
{-

deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

deriving (Show,Read,Eq,Ord,Enum,Bounded,Ix,Generic,NFData,Hashable)

-}
