-- {-# LANGUAGE OverloadedStrings #-}

{-|

@

:set -XOverloadedStrings
sets <- parseSetsFile
let [roe] = sets ^.. _Right . _SetsObject . at "ROE" . _Just 
:i roe
let roe' = roe & setObject_cards .~ [] &  setObject_booster .~ Nothing
roe' 



@

-}
module REPL_Cards_Common
 ( module X  -- REPL
 ) where

----------------------------------------

import MTGJSON      as X
import MTGJSON.Main as X

import Control.Lens   as X hiding
  ((<&>))

import Prelude.Spiros as X hiding
 (Strict, index, at, snoc, uncons)

----------------------------------------
