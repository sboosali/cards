
{-|

-}
module MTGJSON.Extra
 ( module Prelude.Spiros
 , module X
 , module MTGJSON.Extra
 ) where

-- re-exports
import Enumerate          as X (Enumerable)
import Data.List.NonEmpty as X (NonEmpty(..))
import Data.Aeson         as X (eitherDecode)
import Data.Validation    as X (AccValidation(..))

import qualified Data.Aeson        as J 
--import qualified Data.Aeson.Types  as J

import Data.ByteString.Lazy (ByteString) 

import Control.Monad.Fail (MonadFail)

import Control.Lens (Wrapped(..))

import Data.Coerce

import Prelude.Spiros

----------------------------------------

type V = AccValidation

success :: a -> AccValidation e a
success = AccSuccess

failure :: e -> AccValidation (NonEmpty e) a
failure = (:|[]) > AccFailure

maybe2validation :: e -> Maybe a -> AccValidation (NonEmpty e) a
maybe2validation e = maybe (failure e) success

----------------------------------------

type List = []

----------------------------------------

decoded  :: (MonadFail m, J.FromJSON a) => ByteString -> m a
decoded  = J.eitherDecode > either fail return 

----------------------------------------

n2i :: Natural -> Integer
n2i = toInteger

i2n :: Integer -> Maybe Natural
i2n i = if i >= 0
  then Just $ fromInteger i
  else Nothing

----------------------------------------

concatenateA :: (Applicative f) => (a -> f [b]) -> [a] -> f [b]
concatenateA f = traverse f >>> fmap concat

----------------------------------------

show' :: (Show a, StringConv String s) => a -> s
show' = show > toS

----------------------------------------

type Print a = a    -> Text 
type Parse a = Text -> Maybe a 

----------------------------------------

-- | 'coerce' a @newtype@ to its 'Unwrapped' value. 
coerceWrapped
  :: forall a.
    ( Wrapped a
    , Coercible a (Unwrapped a)
    )
  => a
  -> Unwrapped a
coerceWrapped = coerce

-- | 'coerce' an 'Unwrapped' value back to its @newtype@. 
coerceUnwrapped
  :: forall a.
    ( Wrapped a
    , Coercible a (Unwrapped a)
    )
  => Unwrapped a
  -> a
coerceUnwrapped = coerce

----------------------------------------