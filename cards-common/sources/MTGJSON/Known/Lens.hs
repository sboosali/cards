{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE DataKinds #-}

{-|

-}
module MTGJSON.Known.Lens where

import MTGJSON.Extra (concatenateA) 
import MTGJSON.Types 
import MTGJSON.Known.Types 
--import MTGJSON.Known.Newtypes 

--import Enumerate.Between

import Control.Lens hiding ((<&>)) -- (makeLenses,makePrisms) 

----------------------------------------

concatenateA makeLenses
  [ ''Card
  ]
  --NOTE ``Expecting one more argument to ‘CardData’``

concatenateA makePrisms
  [ ''Numeric
  ]

----------------------------------------

----------------------------------------
