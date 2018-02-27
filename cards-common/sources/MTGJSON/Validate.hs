{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.Validate where

import MTGJSON.Extra
--import MTGJSON.Types
import MTGJSON.Known
import MTGJSON.AllSets -- .Types

import MTGJSON.Parser
import MTGJSON.Printer.Finite

import Enumerate.Function (invertInjection)
--import Data.Validation

--import qualified Data.List.NonEmpty as NonEmpty

--import Prelude.Spiros


----------------------------------------

type CardValidation = V CardErrors

type CardErrors = NonEmpty CardError

data CardError
 = MustBeNatural {-String-} Integer
 | MustBeInteger {-String-} Double
 | UnknownColor  {-String-} String
 | BadManaCost String
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)
 --deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

----------------------------------------

validateCardObject :: CardObject -> CardValidation KnownCard
validateCardObject CardObject{..} = _

----------------------------------------

validateNatural :: Int -> CardValidation Natural
validateNatural (toInteger -> i)
  = i2n i
  & maybe (failure e) success 
  where
  e = MustBeNatural i

    -- mconcat [schemaFieldName, " must be a natural (non-negative integer): ", show' i]

-- validateNatural :: String -> Int -> V CardErrors Natural
-- validateNatural schemaFieldName (toInteger -> i)
--   = i2n i
--   & maybe (failure e) success 
--   where
--   e = MustBeNatural schemaFieldName i
--     -- mconcat [schemaFieldName, " must be a natural (non-negative integer): ", show' i]

validateColors :: Maybe (List String) -> CardValidation Colors
validateColors
  = maybe [] id --  -- Set.empty id
  > fmap parseColor'
  > sequenceA
  > fmap fromList
  where
  parseColor' s = s & 
    ( parseColor
    > maybe2validation (UnknownColor s)
    )

  parseColor = invertInjection displayColor

validateManaCost
  :: (Integral i)
  => Maybe String
  -> CardValidation (ManaCost i)
validateManaCost
  = maybe "" id
  > parseManaCost'

  where
  parseManaCost' s
    = s
    & parseManaCost
    & maybe2validation (BadManaCost s)

----------------------------------------

