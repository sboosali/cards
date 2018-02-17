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
--import           Reflex.Dom.Contrib.Widgets.ButtonGroup

import qualified Data.Text as T

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
        (constDyn labeledValues)
        config
  
  labeledValues = constructors proxy
    & fmap (id &&& (show > s2t))

  config = def
    { _widgetConfig_attributes   = dNoAttributes -- constDyn mempty
    , _widgetConfig_initialValue = Nothing -- minBound
--    , _widgetConfig_setValue     = never
    }

  dAttributes = constDyn $
    genericRadioGroupAttributes name
    -- dNoAttributes 

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

-}
