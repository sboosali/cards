{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-| helper widgets for the gui. 

-}
module Cards.Frontend.Widgets where

import Cards.Frontend.Extra
import Cards.Frontend.ButtonGroup (radioGroup)

import Reflex.Dom hiding (Query)

import           Reflex.Dom.Contrib.Widgets.Common
import           Reflex.Dom.Contrib.Widgets.CheckboxList
--import           Reflex.Dom.Contrib.Widgets.ButtonGroup

import qualified Data.Text as T

----------------------------------------

data ToggleCheckboxes
  = SelectEverything
  | DeselectEverything

-- | for 'checkboxList'
fromToggleCheckboxes :: ToggleCheckboxes -> Bool
fromToggleCheckboxes = \case
  SelectEverything   -> True
  DeselectEverything -> False

{-|

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

-- simpleRadioGroup

{-|

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
  => ( MonadJSaddleWidget t m
     )
  => proxy a --TODO DynamicAttributeMap t
  -> m (Dynamic t a)
  -- -> m (HtmlWidget t a)

genericRadioGroup proxy = do
 
  w <- go
  
  let widget = w
       & mapWidget (maybe minBound id)
       & _hwidget_value
  
  return widget
  
  where
  go = divWith dAttributes $ do
      _ <- text name
      radioGroup  --TODO liftJSM?
        (constDyn name)
        (constDyn enumerableValuesWithLabels)
        config
  
  config = def
    { _widgetConfig_attributes   = dNoAttributes -- constDyn mempty
    , _widgetConfig_initialValue = Nothing -- minBound
--    , _widgetConfig_setValue     = never
    }

  dAttributes = constDyn $
    genericRadioGroupAttributes name
    -- dNoAttributes 

  ReifiedEnumerable{enumerableValuesWithLabels} = reifyEnumerable proxy
    -- everything
  
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


-}
