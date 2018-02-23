{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cards.Syntax.MagicCardsInfo.Known where

import Cards.Syntax.MagicCardsInfo.Types

import Enumerate

import Prelude.Spiros
import Prelude (Enum(..))

----------------------------------------

type KnownQuery_ = Query_ KnownSize KnownCost

type KnownSize = Small

type KnownCost = Small

----------------------------------------

newtype Small = Small Int
 deriving (Show,Eq,Ord,Generic,NFData)

minSmall :: Num a => a -- Small
minSmall = -5

maxSmall :: Num a => a -- Small
maxSmall = 25

small :: Int -> Small
small = max minSmall > min maxSmall > Small

instance Enum Small where
  fromEnum (Small i) = i
  toEnum = small

instance Bounded Small where
  minBound = minSmall
  maxBound = maxSmall

instance Enumerable Small where
  enumerated = [minSmall .. maxSmall]

--instance (Integral i, KnownNat n) => Num (i `Mod` n) where

instance Num Small where
  fromInteger = fromInteger > small

  Small i₁ + Small i₂ = small $ i₁ + i₂
  Small i₁ * Small i₂ = small $ i₁ * i₂

  abs    (Small i) = small $ abs i
  signum (Small i) = small $ signum i
  negate (Small i) = small $ negate i

-- newtype KnownSize = KnownSize Int
--  deriving (Num,Show)

-- newtype KnownCost = KnownCost Int
--  deriving (Num,Show)

---------------------------------------
