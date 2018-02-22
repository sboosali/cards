
{-# LANGUAGE CPP #-}

#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif

{-| When the @-fuse-template-haskell@ is enabled, statically\/safely verify the injectivity of functions; otherwise, without @TemplateHaskell@, just dynamically\/unsafely assume so. 

In particular, some of the @display-*@ functions in @"Cards.Syntax.MagicCardsInfo.Printer"@.

Parsers for enums are defined "inverted" (i.e. as printers), for verifying totality via the compiler (checking for exhaustive pattern matching). They can be inverted back again, after verifying injectivity via a macro.

-}
module Cards.Syntax.MagicCardsInfo.Static where

import Cards.Syntax.Extra
import Cards.Syntax.MagicCardsInfo.Types
--import Cards.Syntax.MagicCardsInfo.Printer

import Enumerate
import Enumerate.Function

--import qualified Data.Text.Lazy as T

import Prelude.Spiros 
import Prelude (error)

----------------------------------------
-- `enumerate-function`

{- | @injection@ is a macro ('staticInvertInjective') with @-fuse-template-haskell@, and a normal function without. i.e. can have two completely different types.

-}

-- #ifdef USE_TEMPLATE_HASKELL
-- injection 
--   :: (Enumerable a, Ord a, Ord b)
--   => (a -> b)
--   -> (b -> Maybe a)
-- injection = staticInvertInjective
-- #else
injection 
  :: (Enumerable a, Ord a, Ord b)
  => (a -> b)
  -> (b -> Maybe a)
injection = unsafeInvertInjective
-- #endif

----------------------------------------

#ifdef USE_TEMPLATE_HASKELL
-- | a macro.
staticInvertInjective
  :: (Enumerable a, Ord a, Ord b)
  => (a -> b)
  -> (b -> Maybe a)
staticInvertInjective = _
#endif

----------------------------------------
  
-- | a function.
unsafeInvertInjective
  :: (Enumerable a, Ord a, Ord b)
  => (a -> b)
  -> (b -> Maybe a)
unsafeInvertInjective = isInjective > maybe (error message) id
  where
  message = "[unsafeInvertInjective] given function is not injective"

----------------------------------------



----------------------------------------