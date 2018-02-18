{-# LANGUAGE NoImplicitPrelude #-}

{-|

-}
module MTGJSON.Extra
 ( module Prelude.Spiros
 , module X
 , module MTGJSON.Extra
 ) where

import Data.Aeson as X (eitherDecode)

import qualified Data.Aeson        as J 
--import qualified Data.Aeson.Types  as J

import Data.ByteString.Lazy (ByteString) 

import Control.Monad.Fail (MonadFail)

import Prelude.Spiros 

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