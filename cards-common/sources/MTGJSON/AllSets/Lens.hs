{-# LANGUAGE TemplateHaskell #-}

module MTGJSON.AllSets.Lens where

import MTGJSON.AllSets.Types 
import MTGJSON.Extra (concatenateA) 

import Control.Lens hiding ((<&>)) -- (makeLenses,makePrisms) 

--import Prelude.Spiros

----------------------------------------
-- generated

concatenateA makeLenses
  [ ''CardData
  ]

concatenateA makePrisms
  [ ''CardCharacteristicNumber
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
