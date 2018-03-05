{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

{-|

-}
module MTGJSON.AllSets.Schema where

  -- ( module Schema
  -- ) where

import MTGJSON.AllSets.Object as Object
import MTGJSON.AllSets.Set    as Edition
import MTGJSON.AllSets.Card   as Card

import Prelude.Spiros

----------------------------------------

getEditionCodes :: SetObject -> EditionCodes
getEditionCodes SetObject{..} = EditionCodes{..}
 where
 _Edition_primaryCode        = _SetObject_code
 
 _Edition_gathererCode       = _SetObject_gathererCode
   & fromMaybe _Edition_primaryCode

 _Edition_oldCode            = _SetObject_oldCode
   & fromMaybe _Edition_gathererCode

 _Edition_magicCardsInfoCode = _SetObject_magicCardsInfoCode
   & id
   -- TODO don't default on absence
   -- & fromMaybe _Edition_gathererCode

----------------------------------------

