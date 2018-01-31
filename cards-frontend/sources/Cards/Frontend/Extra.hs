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
 ) where

import Reflex.Dom

import Prelude.Spiros hiding (Text,div)

---------------------------------------

{-|

@
> mainWidget :: SomeWidget () -> IO ()
@

-}
type SomeWidget a = (forall x. Widget x a)

{-|

@
>>> :i Widget
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

>>> :i Widget_
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

{-| (has kind @Constraint@). 

NOTES

@
@

-}  
type HasClick t d = HasEvent t d 'ClickTag

----------------------------------------

-- | @<div>...</div>@
div :: (MonadWidget t m) => m () -> m ()
div = el "div"

-- | the HTML equivalent of the newline @"\n"@. 
div_ :: (MonadWidget t m) => m ()
div_ = div blank

----------------------------------------


{-|

@
domEvent <name>   :: <element> -> Event <type>
domEvent <name> _ ::              Event <type>
@


@
Click :: EventName 'ClickTag
domEvent _ :: Event 
@

@

domEvent Click
  :: HasDomEvent t target 'ClickTag
  => target
  -> Event t (DomEventType target 'ClickTag)

domEvent Click blank
  :: (Monad m, HasDomEvent t (m ()) 'ClickTag)
  => Event t (DomEventType (m ()) 'ClickTag)

domEvent Click
  :: HasDomEvent t target 'ClickTag
  => target
  -> Event t (DomEventType target 'ClickTag)

@

NOTES

@
-- `Element` is the most general element
-- (versus the specialized TextInput, InputElement, TextAreaElement, etc).

instance forall k t (d :: k) (en :: EventTag).
 Reflex t =>
 HasDomEvent t (Element EventResult d t) en

data Element (er :: EventTag -> *) (d :: k) t
 = Element
  { _element_events :: EventSelector t
                      (Data.Functor.Misc.WrapArg er EventName)
  , _element_raw    :: RawElement d
  }

@

-}
--onClick :: DomBuilder t m =>  -> m (Event t ())
onClick
  :: (HasDomEvent t target 'ClickTag)
  => target
  -> Event t (DomEventType target 'ClickTag)
onClick = domEvent Click

{-| specialized 'onClick'.

@
= 'domEvent' 'Click'
@

NOTES

@
> :i El
type El = Element EventResult GhcjsDomSpace :: * -> *

> :i DomEventType
type instance DomEventType (Element EventResult d t) en
  = EventResultType en

> :i EventResultType
type family EventResultType (en :: EventTag) :: *  where
 EventResultType 'ClickTag = ()
 ...

@

so

@
DomEventType (El' d t) 'ClickTag
~
DomEventType (Element EventResult d t) 'ClickTag
~
EventResultType 'ClickTag
~
()
@

-}  
onClick'
  :: (HasClick t d)
  => (El' d t)
  -> Event t ()
onClick' = domEvent Click

----------------------------------------
