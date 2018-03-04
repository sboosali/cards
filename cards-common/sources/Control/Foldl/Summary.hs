{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module Control.Foldl.Summary
  ( module Control.Foldl.Summary
  , module Control.Foldl
  ) where

import qualified "foldl" Control.Foldl as L
import           "foldl" Control.Foldl (Fold(..))
import qualified "foldl" Control.Foldl.Text as LT

import "profunctors" Data.Profunctor

--import Control.Lens hiding (Fold, (<&>))

import qualified Data.Text as T
import           Data.Text (Text)

import qualified Data.Map as Map
import           Data.Map (Map)

import qualified Data.Set as Set
import           Data.Set (Set)

-- import qualified Data.HashMap.Strict as Hash
-- import           Data.HashMap.Strict (HashMap)

import Data.Ratio (Ratio, (%))

import Prelude.Spiros hiding (Text)

----------------------------------------
-- Summarizers

genericlySummarize
  :: ( Foldable f
     , Ord a
     )
  => f a
  -> GenericSummary a
genericlySummarize = L.fold genericFold

numericlySummarize
  :: ( Foldable f
     , Ord a
     , Fractional a
     )
  => f a
  -> NumericSummary a
numericlySummarize = L.fold numericFold

textuallySummarize
  :: (
     )
  => LazyText -- NOTE a pseudo `Foldable f => f Text`
  -> TextualSummary 
textuallySummarize = LT.fold textualFold

----------------------------------------
-- Generic Summaries

type GenericFold a = Fold a (GenericSummary a)

data GenericSummary a = GenericSummary
 { _count     :: Natural
 , _histogram :: Map a Int
 , _uniques   :: Set a

 }

genericFold :: (Ord a) => GenericFold a
genericFold = GenericSummary
  <$> lLength
  <*> lHistogram
  <*> L.set

lLength :: Fold a Natural
lLength = L.genericLength

{- from 'foldl-statistic' -}

-- | Create a histogram of each value of type a. Useful for folding over
-- categorical values, for example, a CSV where you have a data type for a
-- selection of categories.
--
-- It should not be used for continuous values which would lead to a high number
-- of keys. One way to avoid this is to use the `Profunctor` instance for `Fold`
-- to break your values into categories. For an example of doing this, see
-- `ordersOfMagnitude`.
lHistogram :: Ord a => Fold a (Map.Map a Int)
lHistogram = Fold step Map.empty id where
  step m a = Map.insertWith (+) a 1 m

{- from 'foldl-statistic' -}
  
-- -- | Like `histogram`, but for use when hashmaps would be more efficient for the
-- -- particular type @a@.
-- lHistogramHash :: (Hashable a, Eq a) => Fold a (Hash.HashMap a Int)
-- lHistogramHash = Fold step Hash.empty id where
--   step m a = Hash.insertWith (+) a 1 m

----------------------------------------
-- Numerical Summaries

type NullableFold a = Fold a (NullableSummary a)

data NullableSummary (a :: *) = NullableSummary
 { _absence   :: Ratio Natural
 }

nullableFold :: (Monoid m, Eq m) => NullableFold m
nullableFold = NullableSummary
  <$> lEmptyRatio

lEmptyRatio :: (Monoid m, Eq m, Integral n) => Fold m (Ratio n)
lEmptyRatio = (%) <$> lEmpties <*> L.genericLength
--lNullRatio :: (Foldable f) => Fold (f x) Rational  
--lNullRatio :: Fold (Maybe a) Rational

lEmpties :: (Monoid m, Eq m, Num n) => Fold m n -- Natural
lEmpties = lCount mempty

-- lNulls :: (Foldable f) => Fold (f x) Natural
-- lNulls = 

-- | @(count c)@ returns the number of times @c@ appears
lCount :: forall a n. (Eq a, Num n) => a -> Fold a n
lCount x = Fold step 0 id
  where
  step :: n -> a -> n 
  step n y = n + (if x==y then 1 else 0)

----------------------------------------
-- Numerical Summaries

type NumericFold a = Fold a (NumericSummary a)

data NumericSummary a = NumericSummary
 { _minimum :: a
 , _maximum :: a
 , _range   :: a
 , _average :: a
 }

numericFold :: (Ord a, Fractional a) => NumericFold a
numericFold = NumericSummary
  <$> lMinimum1
  <*> lMaximum1
  <*> lRange
  <*> L.mean

-- | The difference between the largest and smallest
-- elements of a sample.
{-# INLINE lRange #-}
lRange :: (Ord a, Num a) => Fold a a
lRange = (-)
  <$> lMaximum1
  <*> lMinimum1

lMinimum1 :: (Ord a, Num a) => Fold a a
lMinimum1 = L.minimum <&> fromMaybe 0

lMaximum1 :: (Ord a, Num a) => Fold a a
lMaximum1 = L.maximum <&> fromMaybe 0

----------------------------------------
-- Textual Summaries

type TextualFold = Fold Text (TextualSummary)

data TextualSummary = TextualSummary
 { _alphabet :: Set Char
 , _shortest :: Natural
 , _longest  :: Natural
 , _nonascii :: Set Text
 --, _asciiWordRatio :: Ratio Natural
 }

textualFold :: TextualFold 
textualFold = TextualSummary
  <$> lAlphabet
  <*> shortest'
  <*> longest'
  <*> lNonAscii
  where
  shortest' = lmap (T.length > unsafeNatural) lMinimum1
  longest'  = lmap (T.length > unsafeNatural) lMaximum1

  unsafeNatural :: Integral i => i -> Natural
  unsafeNatural = fromIntegral

  {-NOTE
    instance Profunctor Fold where
        lmap = premap
        rmap = fmap
   -}


-- |
lNonAscii :: Fold Text (Set Text)
lNonAscii = setFold step 
  where
  step xs y = xs <> ys -- Set.union ts t'
      where
      ys = if T.all isAscii y then [y] else [] -- Set.empty

-- |
lAlphabet :: Fold Text (Set Char)
lAlphabet = L.Fold step Set.empty id
  where
  step :: Set Char -> Text -> Set Char
  step cs t = Set.union cs (Set.fromList (T.unpack t))

----------------------------------------
-- Utilities

setFold :: (Ord a) => (Set b -> a -> Set b) -> Fold a (Set b)
setFold step = L.Fold step Set.empty id

----------------------------------------  
{-

type NullableFold f a = Fold (f a) (NullableSummary f a)
--type NullableFold a = Fold a (NullableSummary a)
--type NullableFold f = forall x. Fold (f x) (NullableSummary (f x))

data NullableSummary (f :: * -> *) (a :: *) = NullableSummary
 { _absence   :: Ratio Natural
 
 }

nullableFold :: (Foldable f) => NullableFold f a
nullableFold = NullableSummary
  <$> lEmptyRatio





numericFold :: NumericFold a
numericFold = NumericSummary
  <$> L.minimum
  <*> L.maximum
  <*> lRange
  <*> L.mean

-- lAverage :: (Fractional a) => Fold a a
-- lAverage = (/) <$> L.sum <*> L.genericLength
-- average xs = sum xs / length xs
  
{- from 'foldl-statistic' -}

-- | The difference between the largest and smallest
-- elements of a sample.
{-# INLINE lRange #-}
lRange :: (Ord a, Num a) => Fold a a
lRange = go <&> fromMaybe 0
  where
  go = (-)
    <$> L.maximum
    <*> L.minimum
 -- go (fromMaybe 0 -> lo) (fromMaybe 0 -> hi)


-}
