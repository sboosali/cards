{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}

{-|

-}
module Cards.Frontend.Extra
 ( module Cards.Frontend.Extra
 , module Prelude.Spiros
 , module Data.Text
-- , module Reflex.Dom
 ) where

import Reflex.Dom hiding (element)

import Data.Text (Text)
import qualified Data.Text as T
import Prelude.Spiros hiding (Text,div)

---------------------------------------

{-|

@
> mainWidget :: SomeWidget () -> IO ()
@

-}
type SomeWidget a = (forall x. Widget x a)

-- | 
type SomeWidget_ = (forall x. Widget x ())

{-|

@
> :i Widget
-- module Reflex.Dom.Main
type Widget x =
  PostBuildT
    DomTimeline
    (ImmediateDomBuilderT
       DomTimeline
       (WithJSContextSingleton
          x
          (PerformEventT DomTimeline DomHost)))
  :: *
  -> *

instance Monad (Widget x)

> :i Widget_
-- module Cards.Frontend.Extra
type Widget_ x =
  PostBuildT DomTimeline
    (ImmediateDomBuilderT DomTimeline
       (WithJSContextSingleton x
          (PerformEventT DomTimeline
             DomHost
             ())))
             
  :: *

type DomTimeline = Spider

type role WithJSContextSingleton phantom representational nominal

@

-}
type Widget_ x = Widget x ()

----------------------------------------

{-| 

NOTES

@
> :i El
type El = Element EventResult GhcjsDomSpace :: * -> *
@

so

@
El ~ El' GhcjsDomSpace
@

-}  
type El' d = Element EventResult d

----------------------------------------

{-| (has kind @'EventTag' -> Constraint@). 

NOTES

@
@

-}  
type HasEvent t d event =
  HasDomEvent t (El' d t) event

----------------------------------------

s2t :: String -> Text
s2t = T.pack

t2s :: Text -> String  
t2s = T.unpack

----------------------------------------

-- | @<div>...</div>@
div :: (MonadWidget t m) => m () -> m ()
div = el "div"

-- | the HTML equivalent of the newline @"\n"@. 
div_ :: (MonadWidget t m) => m ()
div_ = div blank

----------------------------------------

{-| most general

@
= 'elDynAttr''
@

e.g.:

@
> (events, widget) <- elDynAttr "div" (pure $ "style" =: "display:inline-block") blank
@

-}
element
  :: (MonadWidget t m)
  => Text
  -> Dynamic t AttributeMap
  -> m a
  -> m (El' (DomBuilderSpace m) t, a)
element = elDynAttr'

----------------------------------------
