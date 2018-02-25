{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.Core where

import MTGJSON.Extra

import Data.Validation

--import qualified Data.List.NonEmpty as NonEmpty

import Prelude.Spiros

----------------------------------------  

type CardErrors = NonEmpty CardError

data CardError
 = MustBeNatural {-String-} Integer
 | MustBeInteger {-String-} Double
 | UnknownColor  {-String-} Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)
 --deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

----------------------------------------


validateNatural :: Int -> V CardErrors Natural
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

validateColors :: Maybe (List String) -> V CardErrors (Set Color)
validateColors
  = maybe [] id -- Set.empty id
  > fmap parseColor
  > maybe2validation (UnknownColor > failure)
  where
  
    parseColor "White" = pure White
    parseColor "Blue"  = pure Blue
    parseColor "Black" = pure Black
    parseColor "Red"   = pure Red
    parseColor "Green" = pure Green
    parseColor s       = error $ "Unknown color: " <> s

{-

validationNel :: Either e a -> AccValidation (NonEmpty e) a

-}

----------------------------------------
