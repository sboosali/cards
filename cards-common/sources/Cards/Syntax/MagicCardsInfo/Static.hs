
{-# LANGUAGE CPP #-}

#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif

{-|

statically verify the surjectivity of some @display-*@ functions in @"Cards.Syntax.MagicCardsInfo.Printer"@. 

-}
module Cards.Syntax.MagicCardsInfo.Static where

import Cards.Syntax.Extra
import Cards.Syntax.MagicCardsInfo.Types
import Cards.Syntax.MagicCardsInfo.Printer

import Enumerate
import Enumerate.Function

--import qualified Data.Text.Lazy as T

import Prelude.Spiros 

----------------------------------------
-- `enumerate-function`

invertSurjective
  :: (Enumerable a, Ord a, Ord b)
  => (a -> b)
  -> (b -> Maybe a)
#ifdef USE_TEMPLATE_HASKELL
invertSurjective = safeInvertSurjective
#else
invertSurjective = unsafeInvertSurjective
#endif

----------------------------------------

#ifdef USE_TEMPLATE_HASKELL
safeInvertSurjective
  :: (Enumerable a, Ord a, Ord b)
  => (a -> b)
  -> (b -> Maybe a)
#else
safeInvertSurjective = _
#endif

unsafeInvertSurjective
  :: (Enumerable a, Ord a, Ord b)
  => (a -> b)
  -> (b -> Maybe a)
unsafeInvertSurjective = _

----------------------------------------



----------------------------------------