{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-| The user interface.

reflex widgets. 

-}
module Cards.Frontend.GUI where

import Cards.Frontend.Extra
import Cards.Frontend.Types
import Cards.Frontend.DB
import Cards.Frontend.Query
--import Cards.Frontend.Result

import Reflex hiding (Query)
--import qualified Reflex as R
import Reflex.Dom hiding (Query)
import Reflex.Vinyl

--import qualified Control.Lens as L
import Control.Lens hiding ((<&>))

--import qualified Data.Map as Map
import Data.Map (Map)

--import qualified Data.Text as T
--import Data.Text (pack, unpack)

--import Text.Read (readMaybe)
-- import Data.Monoid
-- import System.Environment

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
  styleSheet "css/style.css"

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
             (Click :& Dblclick :& Mousemove :& RNil)
             "div"
             (constDyn mempty)
             child
 
 -- let eClick         = es ^. _Click
 let eMousePosition = es ^. _Mousemove
 let eMousePositionText = eMousePosition
                         <&> (\(x,y) -> show (x,y) & s2t)
     -- i.e. ((s2t . show) <$> eMousePosition)
     
 let eDoubleClickedPositionText = (es^._Dblclick) <&> (show > s2t)

 dMousePosition <- holdDyn "(_,_)" eMousePositionText
 dDoubleClickedPositionText <- holdDyn "(_,_)" eDoubleClickedPositionText

 dynText dMousePosition
 dynText dDoubleClickedPositionText

 return a

----------------------------------------

{-|

-}
wSearchPage :: MonadWidget t m => m ()
wSearchPage = do

  oSearch <- textInput iSearch
  let dSearch = value oSearch
  let dQuery = dSearch
--  let eQuery = never & fmapMaybe nonEmptyString
 
  let dResults = dQuery <&> execQuery defaultCardDatabase

  let dTable = dResults <&> formatResults
  _wTable <- dyn dTable 

  blank

----------------------------------------

initialQuery :: Text
initialQuery = "fire"
 -- initialQuery = "n:fire t:kavu"

placeholderQuery :: Text
placeholderQuery = "fire"

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

formatResults :: (MonadWidget t m) => Results -> m ()
formatResults
 = traverse_ formatCard
 -- = fmap formatCard
 -- > intersperse (sequence_
 --                [div_, text "========================================", div_])
 -- > sequence_

cssDisplayInline :: Map Text Text
cssDisplayInline = "display" =: "inline"

formatCard :: (MonadWidget t m) => Card -> m ()
formatCard Card{..} = elDynAttr "div" (pure cssDisplayInline) $ do
  div $ text _cardName
  div $ text _cardText

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