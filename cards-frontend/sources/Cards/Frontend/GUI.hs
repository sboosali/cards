{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-| The user interface.

reflex widgets.

prototype that searches through card name only, displaying the oracle text too. inline AllCards.json as Haskell literal.  

-}
module Cards.Frontend.GUI where

import Cards.Frontend.Extra
import Cards.Frontend.Types 
import Cards.Frontend.DB (defaultCardDatabase)
import Cards.Frontend.Query (validateQuery)
import Cards.Frontend.Search (runQuery)
--import Cards.Frontend.Result (noResults)

import Reflex hiding (Query)
--import qualified Reflex as R
import Reflex.Dom hiding (Query)
import Reflex.Vinyl

--import qualified Control.Lens as L
import Control.Lens hiding ((<&>))

--import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Text as T
--import Data.Text (pack, unpack)

--import Text.Read (readMaybe)
-- import Data.Monoid
-- import System.Environment

--import Clay hiding (i, s, div, value, (-:))
-- import Clay.Selector (with)
-- import qualified Clay
--import qualified Clay as CSS
import qualified Clay as C

import Data.Time (NominalDiffTime)
--import Data.Thyme.Time -- (NominalDiffTime)
--- ^ NOTE "This module provides compatibility instances and wrappers for the things that thyme does differently from time, and allows it to be used as a drop-in replacement for the latter"

--import Data.Maybe

--import Prelude.Spiros hiding (Text,div)
 -- reflex `Text` is strict
 -- Prelude numerical `div`

----------------------------------------

css :: C.Css -> Text
css = C.render > toS

{-NOTES

-}

----------------------------------------

{-|

Naming:
* "w" for widget.

-}
wHead :: MonadWidget t m => m ()
wHead = do
  el "title" $ text "MTG Card Search"
  styleSheet "css/style.css" --TODO data-files, then post-build cp; `nix`, copy "share", not just "bin", into result-frontend?

  where

  styleSheet url = elAttr "link" (styleAttributes url) blank

  styleAttributes url =
   [ "rel" -: "stylesheet"
   , "type"-: "text/css"
   , "href"-: url
   ] 

{-|

-}
wBody :: MonadWidget t m => m ()
wBody = wSearchPage

----------------------------------------

grid :: SomeWidget_
grid = do
 let child = blank
 
 (es, a) <- elFor'
             (Click :& Dblclick :& RNil)
             "div"
             (constDyn mempty)
             child
 
 -- let eClick         = es ^. _Click
 let eDoubleClickedPositionText = (es^._Dblclick) <&> (show > s2t)

 dDoubleClickedPositionText <- holdDyn "(_,_)" eDoubleClickedPositionText

 dynText dDoubleClickedPositionText

 return a

----------------------------------------

-- | in seconds. 
queryDebouncingInterval :: NominalDiffTime
queryDebouncingInterval = 0.5 --TODO debounce only for display, eagerly compute

{-|

-}
wSearchPage :: MonadWidget t m => m ()
wSearchPage = do

  oSearch <- textInput iSearch
  let dSearchValue = value oSearch

  let eQueryEveryKeypress
        = dSearchValue
        & updated
          -- Continuous
  
  eQueryDebounced <- eQueryEveryKeypress 
    & debounce queryDebouncingInterval
  
  -- dSearchDebounced <- dSearchContinuous
  --   & debounceD searchbarDebouncingInterval ""
  -- let dRawQuery = dSearchDebounced
  
--  let eQuery = never & fmapMaybe nonEmptyString

--                 <&> fromMaybe noResults --NOTE ignore invalid queries

  let (eQueryIsInvalid, eValidQuery)
        = eQueryDebounced
        & fpartitionOnPredicate validateQuery
  
  let eResults = eValidQuery <&> runQuery defaultCardDatabase
  let eSearchError = ("invalid query" <$ eQueryIsInvalid)
  let eLaterResultsPage = leftmost
       [ FailedResultsPage     <$> eSearchError
       , SuccessfulResultsPage <$> eResults
       ]
       --NOTE these are actually mutually-exclusive,
       -- given the `fpartition`

  dResultsPage <- holdDyn InitialResultsPage eLaterResultsPage
  let dWidget = dResultsPage <&> formatResultsPage def
  
  _ <- dyn dWidget

  blank

----------------------------------------

-- wResults :: MonadWidget t m => Event t Results -> m (Dynamic1_ t m)
-- wResults eResults = do

--   dHeader <- holdDyn blank eHeader
--   dTable  <- holdDyn blank eTable
  
-- --  let dSuccess = (dHeader >> dTable)
--   let dSuccess = do
--         dHeader
--         dTable
--   return dSuccess

--   where
--   eCount  = eResults <&> (fromResults > length)
  
--   eHeader = eCount   <&> formatCount 
--   eTable  = eResults <&> formatResults

  -- let eCount   = eResults <&> (fromResults > length)
  
  -- let eHeader = eCount   <&> formatCount 
  -- let eTable  = eResults <&> formatResults

  -- let dSuccess = do
  --      _ <- dyn dHeader
  --      _ <- dyn dTable
  --      blank

  -- dSuccess

----------------------------------------

-- debounceD
--   :: (MonadFix m, MonadHold t m, PerformEvent t m, TriggerEvent t m, MonadIO (Performable m))
--   => NominalDiffTime -> a -> Dynamic t a -> m (Dynamic t a)
-- debounceD duration initial dOriginal = do
--   let eOriginal = dOriginal & updated
-- --  let bOriginal = dOriginal & current

--   eDebounced <- debounce duration eOriginal
  
--   dDebounced <- holdDyn initial eDebounced  -- bOriginal

--   return dDebounced

{-NOTE

debounce
> Block occurrences of an Event until the given number of seconds elapses without
the Event firing, at which point the last occurrence of the Event will fire.

-}


----------------------------------------

initialQuery :: Text
initialQuery = "fire"
 --TODO initialQuery = "n:fire t:kavu"

placeholderQuery :: Text
placeholderQuery = "e.g. fire"

queryAttributes :: Reflex t => Dynamic t (Map Text Text)
queryAttributes = constDyn as
 where
 as = mconcat
     [ "placeholder" =: placeholderQuery  -- /= initialQuery
     ]

 -- as = mempty
 -- as = 
 --     [ "placeholder" -: initialQuery
 --     ]

iSearch :: Reflex t => TextInputConfig t
iSearch = def 
   { _textInputConfig_inputType      = "search"           -- "text"
   , _textInputConfig_attributes     = queryAttributes
  -- , _textInputConfig_initialValue = initialQuery
   }

----------------------------------------
  
-- eResultsOptions :: Event t ResultsOptions
-- eResultsOptions = mergeWith ResultsOptions eResultsFormat eResultsOrder

-- checkboxResultsFormat :: (MonadWidget t m) => AttributeMap -> Event t ResultsFormat  
-- checkboxResultsFormat = checkboxBoundedEnum

-- checkboxResultsOrder :: (MonadWidget t m) => AttributeMap -> Event t ResultsOrder
-- checkboxResultsOrder = checkboxBoundedEnum

----------------------------------------

formatResultsPage
  :: (MonadWidget t m)
  => ResultsOptions -> ResultsPage -> m ()
formatResultsPage _ = \case
 InitialResultsPage       -> formatInitial
 FailedResultsPage e      -> formatError e
 SuccessfulResultsPage rs -> formatSuccess rs

formatInitial
  :: (MonadWidget t m)
  => m ()
formatInitial =
  text "ready"

formatError   
  :: (MonadWidget t m)
  => SearchError
  -> m ()
formatError (SearchError e) =
  text e

formatSuccess
  :: (MonadWidget t m)
  => Results
  -> m ()
formatSuccess results = do
  
  _count  & formatCount
  results & formatResults

  where
  _count = results & (fromResults > length)

----------------------------------------

formatCount :: (MonadWidget t m) => Int -> m ()
formatCount
  = show > T.pack
  > text
  > elClass "div" "card-result-count"
--  > elDynAttr "card-result-count"

formatResults :: (MonadWidget t m) => Results -> m ()
formatResults
 = fromResults
 > traverse_ formatResult
 -- = fmap formatCard
 -- > intersperse (sequence_
 --                [div_, text "========================================", div_])
 -- > sequence_

-- cssDisplayInline :: Map Text Text
-- cssDisplayInline = "display" =: "inline"

attributesCardResult :: Map Text Text
attributesCardResult =
  [ "class" -: "card-result"
  , "style" -: styleCardResult
  ]

styleCardResult :: Text
styleCardResult = T.intercalate " "
  [ "border: 1px solid;"
  , "border-radius: 3px;"
  ]

elementCardResult :: Text
elementCardResult = "div"

formatResult :: (MonadWidget t m) => Result -> m ()
formatResult (Result card) = formatCard card

formatCard :: (MonadWidget t m) => Card -> m ()
formatCard Card{..} = elDynAttr elementCardResult dAttributes $ do
  el "div" $ text _cardName -- span
  el "div" $ text _cardText
  where
  dAttributes = pure attributesCardResult

 -- divClass "card-result" $ do  
 -- div $ text _cardName
 -- div $ text _cardText

-- formatResults :: Results -> Text
-- formatResults
--  = fmap (\Card{..} -> _cardName <> "\n" <> _cardText)
--  > T.intercalate "\n\n========================================\n\n"

----------------------------------------

-- myWidget :: SomeWidget_
-- myWidget = do
--  let child = blank
 
--  (es_1, x) <- elFor'
--              (Click :& Mousemove :& RNil)
--              "div"
--              (constDyn mempty)
--              child
 
--  (es_2)   <- elFor
--              _MousingEvents
--              "div"
--              (constDyn mempty)
--              child

--  -- `_eClick_1 and `_eClick_2` are equivalent
 
--  let _eClick_1     = es_1 ^. event Click 
--  let _eMousemove_1 = es_1 ^. event Mousemove 

--  let _eClick_2     = es_2 ^. _Click 
--  let _eMousemove_2 = es_2 ^. _Mousemove

--  -- let eClick     = ( es ^. (rget Click     . _EventOf) ) 
--  -- let eMousemove = ( es ^. (rget Mousemove . _EventOf) )

--  return x

----------------------------------------

 -- def = TextInputConfig { _textInputConfig_inputType = "text"
 --                        , _textInputConfig_initialValue = ""
 --                        , _textInputConfig_setValue = never
 --                        , _textInputConfig_attributes = constDyn mempty }


{- QuickRef

-- Simplest form.  Create a widget of given type containing the given child.
-- Return whatever the child returns.
[W]   el         :: Text ->                                  m a -> m a

-- This version returns the 'El' as well.
[W]   el'        :: Text ->                                  m a -> m (El, a)

-- These two additionally apply attributes to the element, such as ("class" =: "blah")
[W]   elAttr     :: Text ->            Map Text Text ->      m a -> m a
[W]   elAttr'    :: Text ->            Map Text Text ->      m a -> m (El, a)

-- As above, but now the attribute map is Dynamic
[W]   elDynAttr  :: Text ->   Dynamic (Map Text Text) ->     m a -> m a
[W]   elDynAttr' :: Text ->   Dynamic (Map Text Text) ->     m a -> m (El, a)

-- As above, but with an optional XML namespace for the tag.  Note that this does *not* set the 'xmlns' attribute.  See https://www.w3.org/TR/DOM-Level-2-Core/core.html#ID-DocCrElNS
[W]   elDynAttrNS' :: Maybe Text -> Text -> Dynamic (Map Text Text) -> m a -> m (El, a)

-- Shortcut for elAttr when you only want to set the "class" attribute.
[W]   elClass    :: Text ->                       Text ->    m a -> m a

-}

{-

(<&?>) ::
(<&?>) = flip fmapMaybe
(<&?>) xs p = fmapMaybe p xs


<head>
 <title>MTG Card Search</title>

<meta charset="utf-8">

<meta name="description" content="MTG Card Search. Write smart queries (e.g. 'all creatures their creature types in their text', like lords), and get card results fast. Works offline, and provides some accessibility features like a text-only mode. Currently, the only card game that's indexed is Magic The Gathering.">
<meta name="author" content="Spiros Boosalis">

<meta name="viewport" content="width=device-width, initial-scale=1.0">

<meta property="og:title" content="MTG Card Search">
<meta property="og:image" content="X.png">
<meta property="og:description" content="...">

</head>

-}

----------------------------------------