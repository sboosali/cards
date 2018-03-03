{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}

module MTGJSON.AllSets.Lens where


import MTGJSON.AllSets.Types 
import MTGJSON.AllSets.Schema 
import MTGJSON.Extra (concatenateA) 

import Control.Lens hiding ((<&>)) -- (makeLenses,makePrisms) 

{-
-}

----------------------------------------
-- generated

-- Lenses
concatenateA makeLenses
  [ ''EditionData
  , ''EditionCodes
  ]

-- Prisms
concatenateA makePrisms
  [ ''WhetherOffline
  , ''Booster
  , ''BoosterSlot
  , ''SlotType
  , ''ProbabilityDistribution
  ]  

----------------------------------------
-- generated

-- Lenses
concatenateA makeLenses
  [ ''SetObject
  , ''CardObject
  , ''CardForeignPrintingObject 
  , ''CardRulingObject
  , ''CardFormatLegalityObject
  ]

-- Prisms
concatenateA makePrisms
  [ ''SetsObject
  , ''MagicBoosterSlotObject 
  ]  

----------------------------------------
-- derived

-- -- | 
-- _SimpleLoyalty :: Prism' CardCharacteristicNumber Natural
-- _SimpleLoyalty = _CardCharacteristicLoyalty . _CardNaturalNumber

-- -- | the overwhelming majority of the printed numerical characteristics are just small naturals.
-- _CardNaturalNumber :: Prism' CardNumber Natural
-- _CardNaturalNumber = _CardIntegerNumber . _Natural

----------------------------------------
