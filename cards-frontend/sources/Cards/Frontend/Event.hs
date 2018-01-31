{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}

{-|

for each event in 'EventName', this module defines a specialized accessor for the generic element. 

for example, for @Click@s:

@
type HasClick t d = 'HasEvent' t d 'ClickTag'

onClick
  :: (HasClick t d)
  => ('El'' d t)
  -> Event t ()
onClick = 'domEvent' 'Click'
@

i.e.

@
data 'EventName' (_ :: 'EventTag') where
  'Click'       :: ...
  'Abort'       :: ...
  'Blur'        :: ...
  'Change'      :: ...
  'Click'       :: ...
  'Contextmenu' :: ...
  'Dblclick'    :: ...
  'Drag'        :: ...
  'Dragend'     :: ...
  'Dragenter'   :: ...
  'Dragleave'   :: ...
  'Dragover'    :: ...
  'Dragstart'   :: ...
  'Drop'        :: ...
  'Error'       :: ...
  'Focus'       :: ...
  'Input'       :: ...
  'Invalid'     :: ...
  'Keydown'     :: ...
  'Keypress'    :: ...
  'Keyup'       :: ...
  'Load'        :: ...
  'Mousedown'   :: ...
  'Mouseenter'  :: ...
  'Mouseleave'  :: ...
  'Mousemove'   :: ...
  'Mouseout'    :: ...
  'Mouseover'   :: ...
  'Mouseup'     :: ...
  'Mousewheel'  :: ...
  'Scroll'      :: ...
  'Select'      :: ...
  'Submit'      :: ...
  'Wheel'       :: ...
  'Beforecut'   :: ...
  'Cut'         :: ...
  'Beforecopy'  :: ...
  'Copy'        :: ...
  'Beforepaste' :: ...
  'Paste'       :: ...
  'Reset'       :: ...
  'Search'      :: ...
  'Selectstart' :: ...
  'Touchstart'  :: ...
  'Touchmove'   :: ...
  'Touchend'    :: ...
  'Touchcancel' :: ...
@

-}
module Cards.Frontend.Event where

import Cards.Frontend.Extra

import Reflex.Dom hiding (KeyCode)

--import Prelude.Spiros hiding (Text,div)

---------------------------------------
-- DOM Types

{- |

e.g.:

@
-- the <enter> key
>>> KeyCode 32 
@

-}
newtype KeyCode = KeyCode
 { getKeyCode :: Word
 }

{- |

e.g.:

@
-- 30 "ticks" down?
>>> ScrollDistance (-30) 
@

-}
newtype ScrollDistance = ScrollDistance
 { getScrollDistance :: Double
 }

{- |

@
MousePosition x y
@

e.g.:

@
-- top left corner
>>> MousePosition 0 0 
@

-}
data MousePosition = MousePosition
 { getHorizontalPosition :: Int
 , getVerticalPosition   :: Int
 }

{-| 

e.g.:

@
-- ?
>>> TouchEventResult ... 
@

NOTES

@
-- module Reflex.Dom.Builder.Class.Events
data 'TouchEventResult' = TouchEventResult
 { _touchEventResult_altKey         :: Bool
 , _touchEventResult_ctrlKey        :: Bool
 , _touchEventResult_metaKey        :: Bool
 , _touchEventResult_shiftKey       :: Bool
 , _touchEventResult_changedTouches :: ['TouchResult']
 , _touchEventResult_targetTouches  :: [TouchResult]
 , _touchEventResult_touches        :: [TouchResult]
 }
@

-}
type Touch = TouchEventResult

----------------------------------------

{-

{-| a specialized 'domEvent'.

accesses the click event from an element (for "handlers").

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
onClick
  :: (HasClick t d)
  => (El' d t)
  -> Event t ()
onClick = domEvent Click

{-| (has kind @Constraint@). 

NOTES

@
@

-}  
type HasClick t d = HasEvent t d 'ClickTag

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
onClick'
  :: (HasDomEvent t target 'ClickTag)
  => target
  -> Event t (DomEventType target 'ClickTag)
onClick' = domEvent Click

-}

----------------------------------------

type HasClick       t d = HasEvent t d 'ClickTag

type HasAbort       t d = HasEvent t d 'AbortTag

type HasBlur        t d = HasEvent t d 'BlurTag

type HasChange      t d = HasEvent t d 'ChangeTag

type HasContextmenu t d = HasEvent t d 'ContextmenuTag

type HasDblclick    t d = HasEvent t d 'DblclickTag

type HasDrag        t d = HasEvent t d 'DragTag

type HasDragend     t d = HasEvent t d 'DragendTag

type HasDragenter   t d = HasEvent t d 'DragenterTag

type HasDragleave   t d = HasEvent t d 'DragleaveTag

type HasDragover    t d = HasEvent t d 'DragoverTag

type HasDragstart   t d = HasEvent t d 'DragstartTag

type HasDrop        t d = HasEvent t d 'DropTag

type HasError       t d = HasEvent t d 'ErrorTag

type HasFocus       t d = HasEvent t d 'FocusTag

type HasInput       t d = HasEvent t d 'InputTag

type HasInvalid     t d = HasEvent t d 'InvalidTag

type HasKeydown     t d = HasEvent t d 'KeydownTag

type HasKeypress    t d = HasEvent t d 'KeypressTag

type HasKeyup       t d = HasEvent t d 'KeyupTag

type HasLoad        t d = HasEvent t d 'LoadTag

type HasMousedown   t d = HasEvent t d 'MousedownTag

type HasMouseenter  t d = HasEvent t d 'MouseenterTag

type HasMouseleave  t d = HasEvent t d 'MouseleaveTag

type HasMousemove   t d = HasEvent t d 'MousemoveTag

type HasMouseout    t d = HasEvent t d 'MouseoutTag

type HasMouseover   t d = HasEvent t d 'MouseoverTag

type HasMouseup     t d = HasEvent t d 'MouseupTag

type HasMousewheel  t d = HasEvent t d 'MousewheelTag

type HasScroll      t d = HasEvent t d 'ScrollTag

type HasSelect      t d = HasEvent t d 'SelectTag

type HasSubmit      t d = HasEvent t d 'SubmitTag

type HasWheel       t d = HasEvent t d 'WheelTag

type HasBeforecut   t d = HasEvent t d 'BeforecutTag

type HasCut         t d = HasEvent t d 'CutTag

type HasBeforecopy  t d = HasEvent t d 'BeforecopyTag

type HasCopy        t d = HasEvent t d 'CopyTag

type HasBeforepaste t d = HasEvent t d 'BeforepasteTag

type HasPaste       t d = HasEvent t d 'PasteTag

type HasReset       t d = HasEvent t d 'ResetTag

type HasSearch      t d = HasEvent t d 'SearchTag

type HasSelectstart t d = HasEvent t d 'SelectstartTag

type HasTouchstart  t d = HasEvent t d 'TouchstartTag

type HasTouchmove   t d = HasEvent t d 'TouchmoveTag

type HasTouchend    t d = HasEvent t d 'TouchendTag

type HasTouchcancel t d = HasEvent t d 'TouchcancelTag

----------------------------------------
-- Key Events

onKeydown
 :: (HasKeydown t d, Reflex t)
 => El' d t
 -> Event t KeyCode
onKeydown = domEvent Keydown >>> fmap KeyCode

onKeypress
 :: (HasKeypress t d, Reflex t)
 => El' d t
 -> Event t KeyCode
onKeypress = domEvent Keypress >>> fmap KeyCode

onKeyup
 :: (HasKeyup t d, Reflex t)
 => El' d t
 -> Event t KeyCode
onKeyup = domEvent Keyup >>> fmap KeyCode

----------------------------------------
-- Touch Events

onTouchstart
 :: (HasTouchstart t d, Reflex t)
 => El' d t
 -> Event t TouchEventResult
onTouchstart = domEvent Touchstart

onTouchmove
 :: (HasTouchmove t d, Reflex t)
 => El' d t
 -> Event t TouchEventResult
onTouchmove = domEvent Touchmove

onTouchend
 :: (HasTouchend t d, Reflex t)
 => El' d t
 -> Event t TouchEventResult
onTouchend = domEvent Touchend

onTouchcancel
 :: (HasTouchcancel t d, Reflex t)
 => El' d t
 -> Event t TouchEventResult
onTouchcancel = domEvent Touchcancel

----------------------------------------
-- Mouse Events

onMousemove
 :: (HasMousemove t d, Reflex t)
 => El' d t
 -> Event t MousePosition
onMousemove = domEvent Mousemove >>> fmap (MousePosition&uncurry)

onMousedown
 :: (HasMousedown t d, Reflex t)
 => El' d t
 -> Event t MousePosition
onMousedown = domEvent Mousedown >>> fmap (MousePosition&uncurry)

onMouseup
 :: (HasMouseup t d, Reflex t)
 => El' d t
 -> Event t MousePosition
onMouseup = domEvent Mouseup >>> fmap (MousePosition&uncurry)

onDblclick
 :: (HasDblclick t d, Reflex t)
 => El' d t
 -> Event t MousePosition
onDblclick = domEvent Dblclick >>> fmap (MousePosition&uncurry)

----------------------------------------
-- Mouse Wheel Events

onScroll
 :: (HasScroll t d, Reflex t)
 => El' d t
 -> Event t ScrollDistance
onScroll = domEvent Scroll >>> fmap ScrollDistance

----------------------------------------
-- Unit Events

onClick
  :: (HasClick t d)
  => (El' d t)
  -> Event t ()
onClick = domEvent Click

onAbort
 :: (HasAbort t d)
 => El' d t
 -> Event t ()
onAbort = domEvent Abort

onBlur
 :: (HasBlur t d)
 => El' d t
 -> Event t ()
onBlur = domEvent Blur

onChange
 :: (HasChange t d)
 => El' d t
 -> Event t ()
onChange = domEvent Change

onContextmenu
 :: (HasContextmenu t d)
 => El' d t
 -> Event t ()
onContextmenu = domEvent Contextmenu

onDrag
 :: (HasDrag t d)
 => El' d t
 -> Event t ()
onDrag = domEvent Drag

onDragend
 :: (HasDragend t d)
 => El' d t
 -> Event t ()
onDragend = domEvent Dragend

onDragenter
 :: (HasDragenter t d)
 => El' d t
 -> Event t ()
onDragenter = domEvent Dragenter

onDragleave
 :: (HasDragleave t d)
 => El' d t
 -> Event t ()
onDragleave = domEvent Dragleave

onDragover
 :: (HasDragover t d)
 => El' d t
 -> Event t ()
onDragover = domEvent Dragover

onDragstart
 :: (HasDragstart t d)
 => El' d t
 -> Event t ()
onDragstart = domEvent Dragstart

onDrop
 :: (HasDrop t d)
 => El' d t
 -> Event t ()
onDrop = domEvent Drop

onError
 :: (HasError t d)
 => El' d t
 -> Event t ()
onError = domEvent Error

onFocus
 :: (HasFocus t d)
 => El' d t
 -> Event t ()
onFocus = domEvent Focus

onInput
 :: (HasInput t d)
 => El' d t
 -> Event t ()
onInput = domEvent Input

onInvalid
 :: (HasInvalid t d)
 => El' d t
 -> Event t ()
onInvalid = domEvent Invalid

onMouseenter
 :: (HasMouseenter t d)
 => El' d t
 -> Event t ()
onMouseenter = domEvent Mouseenter

onMouseleave
 :: (HasMouseleave t d)
 => El' d t
 -> Event t ()
onMouseleave = domEvent Mouseleave

onMouseout
 :: (HasMouseout t d)
 => El' d t
 -> Event t ()
onMouseout = domEvent Mouseout

onMouseover
 :: (HasMouseover t d)
 => El' d t
 -> Event t ()
onMouseover = domEvent Mouseover

onMousewheel
 :: (HasMousewheel t d)
 => El' d t
 -> Event t ()
onMousewheel = domEvent Mousewheel

onLoad
 :: (HasLoad t d)
 => El' d t
 -> Event t ()
onLoad = domEvent Load

onSelect
 :: (HasSelect t d)
 => El' d t
 -> Event t ()
onSelect = domEvent Select

onSubmit
 :: (HasSubmit t d)
 => El' d t
 -> Event t ()
onSubmit = domEvent Submit

onBeforecut
 :: (HasBeforecut t d)
 => El' d t
 -> Event t ()
onBeforecut = domEvent Beforecut

onCut
 :: (HasCut t d)
 => El' d t
 -> Event t ()
onCut = domEvent Cut

onBeforecopy
 :: (HasBeforecopy t d)
 => El' d t
 -> Event t ()
onBeforecopy = domEvent Beforecopy

onCopy
 :: (HasCopy t d)
 => El' d t
 -> Event t ()
onCopy = domEvent Copy

onBeforepaste
 :: (HasBeforepaste t d)
 => El' d t
 -> Event t ()
onBeforepaste = domEvent Beforepaste

onPaste
 :: (HasPaste t d)
 => El' d t
 -> Event t ()
onPaste = domEvent Paste

onReset
 :: (HasReset t d)
 => El' d t
 -> Event t ()
onReset = domEvent Reset

onSearch
 :: (HasSearch t d)
 => El' d t
 -> Event t ()
onSearch = domEvent Search

onSelectstart
 :: (HasSelectstart t d)
 => El' d t
 -> Event t ()
onSelectstart = domEvent Selectstart

onWheel
 :: (HasWheel t d)
 => El' d t
 -> Event t ()
onWheel = domEvent Wheel

----------------------------------------


----------------------------------------

 
----------------------------------------  

{-

Click
Abort
Blur
Change
Click
Contextmenu
Dblclick
Drag
Dragend
Dragenter
Dragleave
Dragover
Dragstart
Drop
Error
Focus
Input
Invalid
Keydown
Keypress
Keyup
Load
Mousedown
Mouseenter
Mouseleave
Mousemove
Mouseout
Mouseover
Mouseup
Mousewheel
Scroll
Select
Submit
Wheel
Beforecut
Cut
Beforecopy
Copy
Beforepaste
Paste
Reset
Search
Selectstart
Touchstart
Touchmove
Touchend
Touchcancel

-}
