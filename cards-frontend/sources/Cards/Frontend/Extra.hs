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
 , module X
-- , module Reflex.Dom
 ) where

import Reflex.Dom hiding (element)
--import           GHCJS.DOM.Types            (MonadJSM)

import qualified Data.Text as T

-- re-exports
import Data.String.Conv as X
import Data.Time        as X (NominalDiffTime)
import Data.Text        as X (Text)
import GHCJS.DOM.Types  as X (MonadJSM)

-- re-export custom prelude
import Prelude.Spiros hiding (Text,div)

----------------------------------------

--TODO :: (a -> Bool) -> (a -> Maybe a)

type Predicate a = (a -> Bool)

type Validator a = (a -> Maybe a)
  
{- | for `fmapMaybe`.

-}
fromPredicate :: Predicate a -> Validator a
fromPredicate p = \x -> if p x then Just x else Nothing
  
{- | for `fmapMaybe`.

-}
fromBoolean :: Bool -> Validator a
fromBoolean b = fromPredicate (const b)

---------------------------------------

type IO_ = IO ()

{- | the `reflex-dom-contrib` widgets require @'MonadJSM'@.

-}
type MonadW t m =
  ( MonadWidget t m
  , MonadJSM IO
  )

{-NOTE

MonadJSM m  versus  MonadJSM IO

    • Could not deduce (MonadJSM IO) arising from a use of ‘radioGroup’

-}

---------------------------------------
  
--type DynamicWidget t m

type Dynamic1  t f a = Dynamic t (f a)

type Dynamic1_ t f   = Dynamic t (f ())

type DynamicAttributeMap t = Dynamic t AttributeMap

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

-- -- | enumerate a 'Bounded' 'Enum'. 
-- constructors :: (Enum a, Bounded a) => [a]
-- constructors = [minBound..maxBound]

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
element = elDynAttr' -- <&> fst 

----------------------------------------

-- | @<div>...</div>@
divWith
  :: (MonadWidget t m)
  => Dynamic t AttributeMap -> m a -> m a
divWith = elDynAttr "div"

-- | @<div>...</div>@
div :: (MonadWidget t m) => m a -> m a
div = el "div"

-- | the HTML equivalent of the newline @"\n"@. 
div_ :: (MonadWidget t m) => m ()
div_ = div blank

----------------------------------------

-- |
fpartitionMaybes
  :: FunctorMaybe f
  => f (Maybe a)
  -> (f (), f a)
fpartitionMaybes stream = (failure, success)
  where
  success = stream & fmapMaybe id
  failure = stream & fmapMaybe (fromNothing ())

  fromNothing x = (maybe (Just x) (const Nothing))

-- |
fpartitionEithers
  :: FunctorMaybe f
  => f (Either e a)
  -> (f e, f a)
fpartitionEithers stream = (failure, success)
  where
  success = stream & fmapMaybe isSuccess
  failure = stream & fmapMaybe isFailure
  isSuccess = either (const Nothing) Just
  isFailure = either Just            (const Nothing)

-- |
fpartition
  :: FunctorMaybe f
  => (a -> Bool)
  -> f a
  -> (f a, f a)
fpartition predicate stream = (failure, success)
  where
  success = stream & ffilter predicate
  failure = stream & ffilter (not . predicate)

-- |
fpartitionOn
  :: (Functor f, FunctorMaybe f)
  => (a -> Either e b)
  -> f a
  -> (f e, f b)
fpartitionOn p = fmap p > fpartitionEithers

-- |
fpartitionOnPredicate
  :: (Functor f, FunctorMaybe f)
  => (a -> Maybe b)
  -> f a
  -> (f (), f b)
fpartitionOnPredicate p = fmap p > fpartitionMaybes

-- fpartitionOn split stream = (failure, success)
--   where
--   success = stream & fmapMaybe isSuccess
--   failure = stream & fmapMaybe isFailure

--   isSuccess = split > fromLeft  Nothing 
--   isFailure = split > fromRight Nothing
  
--   -- issuccess = split > either Nothing id
--   -- isFailure = split > either id Nothing

----------------------------------------

fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _        = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b

----------------------------------------

dNoAttributes :: (Reflex t) => Dynamic t AttributeMap
dNoAttributes = constDyn mempty

----------------------------------------
