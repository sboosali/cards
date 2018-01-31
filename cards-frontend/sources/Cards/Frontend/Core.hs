{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-| The core definitions. 

-}
module Cards.Frontend.Core where

import Cards.Frontend.Extra
import Cards.Frontend.Types

import Reflex hiding (Query)
--import qualified Reflex as R
import Reflex.Dom hiding (Query)

--import qualified Control.Lens as L

--import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Text as T
import Data.Text (Text)
--import Data.Text (pack, unpack)

--import Text.Read (readMaybe)
-- import Data.Monoid
-- import System.Environment

{-NOTES

create an HTML element, with (1) Dynamic attributes and a (2) child element, returning its Events:

    elDynAttr'
     :: Text
     -> Dynamic (Map Text Text)
     -> m a 
     -> m (El, a)

elDynAttr' ~ elDynAttr' NoXMLNamespace :

    elDynAttr'
     = elDynAttrNS' Nothing

elDynAttr ~ snd <&> elDynAttr' :

    elDynAttr tag dAttributes child
     = snd <$> elDynAttr' tag dAttributes child

elDynClass ~ ("class" =:) <$> elDynAttr :

    elDynClass tag dClass
     = elDynAttr tag (("class" =:) <$> dClass)

elAttr ~ pure >>> elDynAttr :

    elAttr tag attributes child
     = elDynAttr tag (pure attributes) child
    

type El = Element EventResult GhcjsDomSpace



module Reflex.Dom.Widget.Basic

button :: DomBuilder t m => Text -> m (Event t ())
button t = do
  (e, _) <- element "button" def $ text t
  return $ domEvent Click e

i.e.

button :: (... t m) => Text -> m (Event t ())
button t = do
  (e, _) <- element "button" def $ text t
  let eClick = domEvent Click e 
  return eClick

button' :: MonadWidget t m => Text -> m ( t ())
button' t = do
  (esButton, _) <- element "button" def $ text t
  -- let eClick = domEvent Click esButton
  return esButton



  Abort :: EventName 'AbortTag
  Blur :: EventName 'BlurTag
  Change :: EventName 'ChangeTag
  Click :: EventName 'ClickTag
  Contextmenu :: EventName 'ContextmenuTag
  Dblclick :: EventName 'DblclickTag
  Drag :: EventName 'DragTag
  Dragend :: EventName 'DragendTag
  Dragenter :: EventName 'DragenterTag
  Dragleave :: EventName 'DragleaveTag
  Dragover :: EventName 'DragoverTag
  Dragstart :: EventName 'DragstartTag
  Drop :: EventName 'DropTag
  Error :: EventName 'ErrorTag
  Focus :: EventName 'FocusTag
  Input :: EventName 'InputTag
  Invalid :: EventName 'InvalidTag
  Keydown :: EventName 'KeydownTag
  Keypress :: EventName 'KeypressTag
  Keyup :: EventName 'KeyupTag
  Load :: EventName 'LoadTag
  Mousedown :: EventName 'MousedownTag
  Mouseenter :: EventName 'MouseenterTag
  Mouseleave :: EventName 'MouseleaveTag
  Mousemove :: EventName 'MousemoveTag
  Mouseout :: EventName 'MouseoutTag
  Mouseover :: EventName 'MouseoverTag
  Mouseup :: EventName 'MouseupTag
  Mousewheel :: EventName 'MousewheelTag
  Scroll :: EventName 'ScrollTag
  Select :: EventName 'SelectTag
  Submit :: EventName 'SubmitTag
  Wheel :: EventName 'WheelTag
  Beforecut :: EventName 'BeforecutTag
  Cut :: EventName 'CutTag
  Beforecopy :: EventName 'BeforecopyTag
  Copy :: EventName 'CopyTag
  Beforepaste :: EventName 'BeforepasteTag
  Paste :: EventName 'PasteTag
  Reset :: EventName 'ResetTag
  Search :: EventName 'SearchTag
  Selectstart :: EventName 'SelectstartTag
  Touchstart :: EventName 'TouchstartTag
  Touchmove :: EventName 'TouchmoveTag
  Touchend :: EventName 'TouchendTag
  Touchcancel :: EventName 'TouchcancelTag


{- |

>>> KeyCode 32 -- the <enter> key

-}
newtype KeyCode = KeyCode
 { getKeyCode :: Word
 }

{- |

>>> ScrollDistance (-30) -- 30 "ticks" down?

-}
newtype ScrollDistance = ScrollDistance
 { getScrollDistance :: Double
 }

{- |

>>> MousePosition x y

-}
data MousePosition = MousePosition
 { getHorizontalPosition :: Int
 , getVerticalPosition   :: Int
 }

{-|

@
-- module Reflex.Dom.Builder.Class.Events
data TouchEventResult = TouchEventResult
 { _touchEventResult_altKey         :: Bool
 , _touchEventResult_ctrlKey        :: Bool
 , _touchEventResult_metaKey        :: Bool
 , _touchEventResult_shiftKey       :: Bool
 , _touchEventResult_changedTouches :: [TouchResult]
 , _touchEventResult_targetTouches  :: [TouchResult]
 , _touchEventResult_touches        :: [TouchResult]
 }
@

-}
type Touch = TouchEventResult

EventResultType 'KeypressTag = Word
  EventResultType 'KeydownTag = Word
  EventResultType 'KeyupTag = Word

EventResultType 'ScrollTag = Double

  EventResultType 'DblclickTag = (Int, Int)
  EventResultType 'MousemoveTag = (Int, Int)
  EventResultType 'MousedownTag = (Int, Int)
  EventResultType 'MouseupTag = (Int, Int)

EventResultType 'TouchstartTag = TouchEventResult
  EventResultType 'TouchmoveTag = TouchEventResult
  EventResultType 'TouchendTag = TouchEventResult
  EventResultType 'TouchcancelTag = TouchEventResult

EventResultType 'ClickTag = ()
  EventResultType 'MouseenterTag = ()
  EventResultType 'MouseleaveTag = ()
  EventResultType 'FocusTag = ()
  EventResultType 'BlurTag = ()
  EventResultType 'ChangeTag = ()
  EventResultType 'DragTag = ()
  EventResultType 'DragendTag = ()
  EventResultType 'DragenterTag = ()
  EventResultType 'DragleaveTag = ()
  EventResultType 'DragoverTag = ()
  EventResultType 'DragstartTag = ()
  EventResultType 'DropTag = ()
  EventResultType 'AbortTag = ()
  EventResultType 'ContextmenuTag = ()
  EventResultType 'ErrorTag = ()
  EventResultType 'InputTag = ()
  EventResultType 'InvalidTag = ()
  EventResultType 'LoadTag = ()
  EventResultType 'MouseoutTag = ()
  EventResultType 'MouseoverTag = ()
  EventResultType 'MousewheelTag = ()
  EventResultType 'SelectTag = ()
  EventResultType 'SubmitTag = ()
  EventResultType 'BeforecutTag = ()
  EventResultType 'CutTag = ()
  EventResultType 'BeforecopyTag = ()
  EventResultType 'CopyTag = ()
  EventResultType 'BeforepasteTag = ()
  EventResultType 'PasteTag = ()
  EventResultType 'ResetTag = ()
  EventResultType 'SearchTag = ()
  EventResultType 'SelectstartTag = ()
  EventResultType 'WheelTag = ()



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

----------------------------------------

{-|

Naming:
* "w" for widget.

-}
wBody :: MonadWidget t m => m ()
wBody = do

  oSearch <- textInput iSearch
  let dSearch = value oSearch
  let dQuery = dSearch -- & fmapMaybe nonEmptyString
 
  let dResults = dQuery <&> execQuery defaultCardDatabase

  let dTable = dResults <&> formatResults
  _wTable <- dyn dTable 

  blank

  --where
  --nonEmptyString = fromPredicate T.null

{- | for `fmapMaybe`

-}
fromPredicate :: (a -> Bool) -> (a -> Maybe a)
fromPredicate p = \x -> if p x then Just x else Nothing

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

execQuery :: CardDatabase -> Query -> Results
execQuery db = parseQuery >>> runQuery db 

-- execQuery :: CardDatabase -> Query -> Text
-- execQuery db = parseQuery >>> runQuery db >>> formatResults 

parseQuery :: Text -> Query 
parseQuery q = T.toLower q

runQuery :: CardDatabase -> Query -> Results
runQuery db q = db & filter (\Card{..} -> q `T.isInfixOf` _cardName) 

 -- (_cardName == q) 
 -- & fmap _cardText

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

defaultCardDatabase :: CardDatabase
defaultCardDatabase = fmap (Card & uncurry)
 [ "fire" -: "deal 3 damage", "ice" -: "tap target permanent", "kavu firemaw" -: "FIREMAW" ]

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