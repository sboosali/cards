-- {-# LANGUAGE OverloadedStrings #-}

{-|

sessions:

@

:set -XOverloadedStrings
:set +t
sets <- parseSetsFile
let [roe] = sets ^.. _Right . _SetsObject . at "ROE" . _Just 
:i roe
let roe' = roe & setObject_cards .~ [] & setObject_booster .~ Nothing
roe' 
boosters = sets ^.. _Right . _SetsObject . traverse . setObject_booster . _Just . traverse . to fromMagicBoosterSlotObject :: [[Text]]
import qualified Data.Set as Set
slots = boosters & fmap Set.fromList & Set.fromList & toList :: [Set Text]
slottables = boosters & concat & Set.fromList & Set.toList :: [Text]



@

results:

@

> slots & toList
[fromList ["Steamflogger Boss","land"],fromList ["checklist","land"],fromList ["checklist","marketing"],fromList ["common"],fromList ["common","double faced mythic rare","double faced rare"],fromList ["common","timeshifted common"],fromList ["double faced"],fromList ["double faced common","double faced uncommon"],fromList ["draft-matters"],fromList ["foil","power nine"],fromList ["foil common","foil mythic rare","foil rare","foil uncommon"],fromList ["land"],fromList ["marketing"],fromList ["mythic rare","rare"],fromList ["rare"],fromList ["rare","timeshifted rare"],fromList ["rare","uncommon"],fromList ["timeshifted common"],fromList ["timeshifted purple"],fromList ["timeshifted rare","timeshifted uncommon"],fromList ["timeshifted uncommon","uncommon"],fromList ["token"],fromList ["uncommon"],fromList ["urza land"]]
it :: [Set Text]

> slottables = boosters & concat & Set.fromList & Set.toList :: [Text]
["Steamflogger Boss","checklist","common","double faced","double faced common","double faced mythic rare","double faced rare","double faced uncommon","draft-matters","foil","foil common","foil mythic rare","foil rare","foil uncommon","land","marketing","mythic rare","power nine","rare","timeshifted common","timeshifted purple","timeshifted rare","timeshifted uncommon","token","uncommon","urza land"]

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
 (Strict, index, at, snoc, uncons,Format)

----------------------------------------
