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
  [ ''Editions
  
    -- records
  , ''Edition
  , ''EditionCodes
  ]

----------------------------------------
-- Prisms (generated)

concatenateA makePrisms
  [ ''Edition

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
