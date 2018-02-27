{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.Core where

import MTGJSON.Extra
import MTGJSON.Types
import MTGJSON.Known

import MTGJSON.Parser
--import MTGJSON.Printer

--import Enumerate.Function (invertInjection)
import Data.Validation

--import qualified Data.List.NonEmpty as NonEmpty

import Prelude.Spiros

----------------------------------------

fromCardObject :: CardObject -> KnownCard
fromCardObject CardObject{..} = _

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

validateColors :: Maybe (List String) -> V CardErrors Colors
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

validateManaCost :: Maybe String -> CardValidation (List ManaSymbol)
validateManaCost = \case
  Nothing -> success []
  Just s  -> case runParser s $ many manaSymbol of
    Left err -> 
    Right manaCost -> pure manaCost

----------------------------------------

