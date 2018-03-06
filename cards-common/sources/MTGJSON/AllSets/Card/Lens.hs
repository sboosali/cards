{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}

{-|


-}
module MTGJSON.AllSets.Card.Lens where

import MTGJSON.Extra (concatenateA)

import MTGJSON.AllSets.Card.Schema

import Control.Lens hiding ((<&>))

----------------------------------------
-- Lenses (generated)

concatenateA makeLenses
  [ ''CardSchema

    -- records
  , ''NumericSchema
  , ''CreatureLike
  , ''PlaneswalkerLike
  , ''VanguardLike

    -- records
  , ''ForeignPrinting 
  , ''FormatLegality
  , ''Ruling
  
  ]

----------------------------------------
-- Prisms (generated)

concatenateA makePrisms
  [ ''UniqueID
  , ''MultiverseID

    -- newtypes
  -- , ''ManaCost
  -- , ''Border
  -- , ''Color
  -- , ''EditionName
  -- , ''Supertype
  -- , ''Cardtype
  -- , ''Subtype
  -- , ''Rarity
  , ''Flavor
  , ''Artist
  , ''CollectorNumber

    -- newtypes
  -- , ''Language
  -- , ''CardName
  -- , ''Format
  -- , ''Legality

    -- enums
  , ''IsTimeshifted
  , ''IsReserved
  , ''IsStarter
  
  -- , ''WhetherOffline
  -- , ''Booster
  -- , ''BoosterSlot
  -- , ''ProbabilityDistribution
  -- , ''GenericSlot'
  -- , ''GenericSlot
  ]  

----------------------------------------
-- Optics (derived)


----------------------------------------
