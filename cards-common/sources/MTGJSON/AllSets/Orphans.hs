{-# LANGUAGE InstanceSigs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-| Orphan instances:

* @instance 'Hashable' 'Day'@

Oprhans are unfortunately necessary when authors don't instantiate typeclasses in boot libraries like `deepseq`, or in ubiquitous (which is somewhat subjective) ones like `hashable`.

Usage:

@
    \{-# OPTIONS_GHC -fno-warn-orphans #-}
    ...
    import MTGJSON.AllSets.Orphans()
@

-}
module MTGJSON.AllSets.Orphans where

import "hashable" Data.Hashable

import "thyme" Data.Thyme.Calendar

import "base" Prelude

----------------------------------------

instance Hashable Day where
  hashWithSalt :: Int -> Day -> Int
  hashWithSalt salt (ModifiedJulianDay i) = hashWithSalt salt i

----------------------------------------
