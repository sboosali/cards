{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}

{-|


-}
module MTGJSON.AllSets.Set.Lens where

import MTGJSON.Extra (concatenateA)

import MTGJSON.AllSets.Set.Schema

import Control.Lens hiding ((<&>))

----------------------------------------
-- Lenses (generated)

concatenateA makeLenses
  [ ''Edition
  
    -- records
  , ''EditionCodes
  ]

----------------------------------------
-- Prisms (generated)

concatenateA makePrisms
  [ ''Editions

    -- newtypes
  , ''BlockName
  , ''EditionType

    -- newtypes
  , ''Booster
  , ''BoosterSlot
   
    -- enums
  , ''WhetherOffline
  ]  

----------------------------------------
-- Optics (derived)


----------------------------------------
