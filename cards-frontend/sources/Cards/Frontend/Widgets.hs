{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels, DuplicateRecordFields #-}

{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecursiveDo         #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists   #-}

{-| helper widgets for the gui. 

-}
module Cards.Frontend.Widgets where

import Cards.Frontend.Extra
--import Cards.Frontend.ButtonGroup (radioGroup)

import Reflex.Dom hiding (Query, value, setValue, attributes)
import Reflex.Vinyl  --TODO mv/rename dom-specific to to Reflex.Dom.Vinyl

import           Reflex.Dom.Contrib.Widgets.Common (HtmlWidget(..))
import           Reflex.Dom.Contrib.Widgets.CheckboxList
--import           Reflex.Dom.Contrib.Widgets.ButtonGroup

import qualified Data.Text as T
import Control.Lens hiding ((<&>))

----------------------------------------

data ToggleCheckboxes
  = SelectEverything
  | DeselectEverything

-- | for 'checkboxList'
fromToggleCheckboxes :: ToggleCheckboxes -> Bool
fromToggleCheckboxes = \case
  SelectEverything   -> True
  DeselectEverything -> False

{-| A simple widget to choose multiple value of some type.

It's automatically derived from the type; the appearence\/behavior won't be optimal, but it's useful for prototyping. 

This widgets creates:
* a checkbox for every value in an finitely 'Enum'erable type, with labels from 'Show'.
* two extra checkboxes to conveniently select/deselect everything (i.e. every other checkbox).
* a text input to filter these checkboxes by name.

The group (of checkboxes) is labeled after the type (i.e. the label @"Bool"@ if @a ~ Bool@).

-}
simpleSetWidget
  :: forall a m t.
     ( Bounded  a
     , Enum     a
     , Show     a
     , Ord      a
     , Typeable a
     )
  => ( MonadWidget t m
     )
  => (Set a)
  -- ^ the initially checked values
  -> m (Dynamic t (Set a))
  -- ^ the (dynamically-changing) currently checked values
simpleSetWidget checkedValues = do
  
  let newCheckboxes = checkboxList
          (show > fromString)
          keepAnyValueAnyLabel
          (eToggleCheckboxes <&> fromToggleCheckboxes)
          dSearch
          checkedValues
          displayedValues

  --TODO
  -- let newToggleCheckboxes = ()
  -- let newSearchBar = ()

  wCheckboxes <- newCheckboxes
  let eChecked = (wCheckboxes & _hwidget_change) <&> fromList
  
  dChecked <- holdDyn checkedValues eChecked
  
  return dChecked

  where
  
  displayedValues = enumerableValues
  ReifiedEnumerable{enumerableValues} =
    reifyEnumerable_ proxy -- everything
  proxy = checkedValues -- 
  
  keepAnyValueAnyLabel = true2
  true2 :: forall x y. x -> y -> Bool
  true2 = (const.const) True

  eToggleCheckboxes :: Event t ToggleCheckboxes
  eToggleCheckboxes = never

  dSearch :: Dynamic t Text
  dSearch = constDyn ""

{-

  let dChecked = (wCheckboxes & _hwidget_values)
        <&> fromList


  let HtmlWidget{_hwidget_value = dChecked} = wCheckboxes

-}

----------------------------------------

{-|

-}
simpleNumberInput
  :: forall a m t.
     ( Num  a
     , Show a
     , Read a
     )
  => ( MonadWidget t m
     )
  => a -- ^ the initial value
  -> m (Dynamic t a)
simpleNumberInput i = el "div" $ do
  oInput <- textInput config
  let eText = oInput & _textInput_value & updated

  let eNumber = eText & fmapMaybe parseNumber
  dNumber <- holdDyn i eNumber

  return dNumber

  where
  parseNumber = T.unpack > readMay
  
  config = def
    { _textInputConfig_inputType    = "number"
    , _textInputConfig_initialValue = show i & s2t
    }

----------------------------------------

-- simpleRadioGroup

{-| A simple widget to choose a single value of some type.

It's automatically derived from the type; the appearence\/behavior won't be optimal, but it's useful for prototyping. 
 
A radio button for very value in an finitely 'Enum'erable type,
with labels from being 'Show'n. The radio group is named after the type (i.e. @@a@).

the initial/default element is 'minBound'; e.g. when 
@deriving (Enum,...), it's the first (lexically) constructor. 

-}
genericRadioGroup  
  :: forall a m t proxy.
     ( Bounded  a
     , Enum     a
     , Show     a
     , Eq       a
     , Typeable a
     -- , Default a --TODO def or minBound?
     )
  => ( MonadWidget t m
     )
  => proxy a --TODO DynamicAttributeMap t
  -> m (Dynamic t a)
  -- -> m (HtmlWidget t a)

genericRadioGroup proxy = do
  dValue <- go
  return dValue
  
  where
  go = divWith dAttributes $ do
      _ <- text name
      simpleRadioGroup 
        name
        show'
        show'
        config

  show' = show > s2t
  config = enumerableRadioConfig 
  
  dAttributes = constDyn $
    genericRadioGroupAttributes name
  
  name = typeName proxy -- typeRep proxy & s2t
  -- proxy = (Proxy :: Proxy a)

genericRadioGroupAttributes :: Text -> Map Text Text
genericRadioGroupAttributes name =
  [ "class" -: name
  , "style" -: style
  ]
  where
  style = T.intercalate " "
    [ "border: 1px solid;"
    , "border-radius: 3px;"
    ]

genericRadioButtonAttributes :: Text -> Map Text Text
genericRadioButtonAttributes name = 
  [ "class" -: name
  , "style" -: style
  ]
  where
  style = T.intercalate " "
    [ "border: 1px solid;"
    , "border-radius: 3px;"
    ]

----------------------------------------

-- data RadioConfig t a = RadioConfig
--  { _radioConfig_initialValue :: a
--  , _radioConfig_setValue     :: Event t a
-- -- , _radioConfig_ 
--  }

data RadioConfig t a = RadioConfig
 { initialValue   :: a
 , possibleValues :: [a]
 , setValue       :: Event   t a
 , setAttributes  :: Dynamic t (AttributeMap)
-- , _radioConfig_ :: a
 } deriving (Functor)
 
instance (Default a, Reflex t) => Default (RadioConfig t a) where
  def = defaultRadioConfig (def:|[])
  -- def = RadioConfig def never (constDyn mempty)
  -- def = RadioConfig def [def] never def
  -- def = RadioConfig def constructors' never def

defaultRadioConfig :: (Reflex t) => NonEmpty a -> RadioConfig t a
defaultRadioConfig (x:|xs) = RadioConfig x (x:xs) never dNoAttributes
  -- RadioConfig{..}

enumerableRadioConfig 
  :: forall t a.
     ( Bounded  a
     , Enum     a
     )
  => ( Reflex t
     )
  => RadioConfig t a
enumerableRadioConfig = defaultRadioConfig constructors1'
  -- { initialValue   = enumerableFirstValue
  -- , possibleValues = enumerableValues
  -- }
  -- where
  -- ReifiedEnumerable{..} = reifyEnumerable_ Nothing

------------------------------------------------------------------------------

{-| A simple widget for a group of labeled buttons.

Features:

TODO * Keyboard Navigation/Control: pressing the up\/down arrow keys
moves the focus to the previous\/next button; pressing the enter key selects the focused button.

e.g. select an 'Ordering' (also see the MDN examples of radio groups)

This reflex widget

@
orderingRadioGroup = simpleRadioGroup
  "comparator"
  show
  displayOrdering
  'enumerableRadioConfig'
  where
  displayOrdering = \case
    LT -> "Strictly Less    Than (x < y)"
    EQ -> "Exactly  Equal   To   (x = y)"
    GT -> "Strictly Greater Than (x > y)"
@

has (inferred) type

@
orderingRadioGroup :: (...) => m (Dynamic t Ordering)
@

and produces this html:

@
<form> 
  <div>

    <input type="radio" name="comparator" id="comparator_1" value="LT">
    <input type="radio" name="comparator" id="comparator_2" value="EQ">
    <input type="radio" name="comparator" id="comparator_3" value="GT">

    <label for="comparator_1">Strictly Less    Than (x < y)</label>
    <label for="comparator_2">Exactly  Equal   To   (x = y)</label>
    <label for="comparator_3">Strictly Greater Than (x > y)</label>

  </div>
</form> 
@

where the dynamic @Dynamic t Ordering@ is whichever was clicked. 

-}
simpleRadioGroup
  :: MonadWidget t m
  => Text
  -> (a -> Text)
  -> (a -> Text)
  -> RadioConfig t a
  -> m (Dynamic t a)
simpleRadioGroup _groupLabel _show' display' RadioConfig{..} = do

 esClicked <- makeButtons'

 let eClicked = esClicked & leftmost
 dValue <- holdDyn initialValue eClicked
 return dValue

 where
 --dAttributes = _
 makeButtons' = el "div" $ do
   possibleValues & traverse button'
-- button' :: 
 button' x = buttonOf (display' x) x
 -- ePressUpArrow   = never
 -- ePressDownArrow = never
 -- ePressEnter     = never
 
 -- f2 :: Rec I '[a,()] -> a
 -- f2 = (\(Identity a :& Identity () :& RNil) -> a) --TODO view (_2)

buttonOf
  :: MonadWidget t m
  => Text
  -> a
  -> m (Event t a)
buttonOf t x = do
   events <- simpleButton t
   let eClick = events ^. _Click -- events & view _Click
   let eValue = eClick <&> const x
   return eValue





-- simpleRadioGroup
--   :: MonadWidget t m
--   => Text
--   -> (a -> Text)
--   -> RadioConfig t a
--   -> m (Dynamic t a)
-- simpleRadioGroup label show' RadioConfig{..} = do
--  esTagged <- possibleValues & traverse button'
--  let esClicked = esTagged <&> fmap f2
--  let eClicked  = esClicked & leftmost
--  dValue <- holdDyn initialValue eClicked
--  return dValue
--  where
-- -- button' :: 
--  button' x = buttonOf (show' x) x
--  -- ePressUpArrow   = never
--  -- ePressDownArrow = never
--  -- ePressEnter     = never
--  f2 :: Rec I '[a,()] -> a
--  f2 = (\(Identity a :& Identity () :& RNil) -> a) --TODO view (_2)
 
-- buttonOf
--   :: MonadWidget t m
--   => Text
--   -> a
--   -> m (Rec I '[a,()])
-- buttonOf t x = do
--    events <- simpleButton t
--    let tagged = events & _Click %~ fmap (const x)
--    return tagged

--TODO
    -- • Could not deduce (Field2
    --                       (Rec I '[a, ()]) (Rec I '[a, ()]) (Event t0 a0) (Event t0 a0))
    --     arising from a use of ‘_2’

{-


simpleRadioGroup label show' RadioConfig{..} = do
 taggedEvents <- possibleValues & traverse (pure &&& button')
 let esClick = taggedEvents <&> view (_2 . _Click)
 let eValue  = esClick & _
 dValue
 where
 dValue = _
 button' x = do
   simpleButton t
   where
   t = show' x

-}

------------------------------------------------------------------------------

{-| A simple widget for a single button. 

-}  
simpleButton
  :: MonadWidget t m
  => Text
  -> m (DOMEvents t [ClickTag,FocusTag])
simpleButton label = do
  (es, _) <- elFor'
             (Click :& Focus :& RNil)
             "button" 
             dAttributes
             (text label)

  return es
  
  where
  dAttributes = dNoAttributes -- constDyn def

----------------------------------------
  
{-NOTES

-- | Generalized form of many widget functions.
type GWidget t m a = WidgetConfig t a -> m (HtmlWidget t a)


-- | A general-purpose widget return value.
data HtmlWidget t a = HtmlWidget
    { _hwidget_value    :: Dynamic t a
      -- ^ The authoritative value for this widget.
    , _hwidget_change   :: Event t a
      -- ^ Event that fires when the widget changes internally (not via a
      -- setValue event).
    , _hwidget_keypress :: Event t Int
    , _hwidget_keydown  :: Event t Int
    , _hwidget_keyup    :: Event t Int
    , _hwidget_hasFocus :: Dynamic t Bool
    }



-- | Takes a list of labels to make checkboxes for and returns the labels of
-- the boxes that are checked.
checkboxList
    :: forall t m a. (MonadWidget t m, Ord a)
    => (a -> Text)
    -- ^ Function to show each item
    -> (Text -> a -> Bool)
    -- ^ Function to filter each item
    -> Event t Bool
    -- ^ Blanket event to apply to all list items.  Allows you to have "select
    -- all" and "select none" buttons.  Fire True to select all and False to
    -- select none.
    -> Dynamic t Text
    -- ^ A search string for filtering the list of items.
    -> Set a
    -- ^ Set of items that should be initially checked
    -> [a]
    -- ^ List of items to show checkboxes for
    -> m (HtmlWidget t [a])
    -- ^ Dynamic list of checked items
checkboxList showFunc filterFunc blanketEvent searchString onItems items = do


  -- -> Event t ToggleCheckboxes
  -- -- ^ 


-- | A widget to construct a tabbed view that shows only one of its child widgets at a time.
--   Creates a header bar containing a <ul> with one <li> per child; clicking a <li> displays
--   the corresponding child and hides all others.
tabDisplay :: forall t m k. (MonadFix m, DomBuilder t m, MonadHold t m, PostBuild t m, Ord k)
  => Text               -- ^ Class applied to <ul> element
  -> Text               -- ^ Class applied to currently active <li> element
  -> Map k (Text, m ()) -- ^ Map from (arbitrary) key to (tab label, child widget)
  -> m ()
tabDisplay ulClass activeClass tabItems = do





-}
