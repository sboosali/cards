-- {-# LANGUAGE OverloadedStrings #-}

{-| Internal module.

Exports definitions and utitilies for interactive development in the REPL. 


examples:

@
Right (SetsObject os) <- readDataFile AllDataFile >>= (pSetsObject>return) >>= evaluate 

@


sessions:

@






Just os <- parseSetsFile


valids ^. _Editions . to length
74

(invalids, valids) = validateEditions os

vs = valids ^.. _Editions . traverse . edition_name . _EditionName
vs & traverse_ (toS > putStrLn)

length invalids
146

invalids ^.. traverse . setObject_name

is = invalids & traverse . setObject_cards .~ []
is & traverse_ print

isRealEdition t = t `elem` (realEditionNames ^.. traverse . _EditionName)  -- :: Text -> Bool 
rs = is ^.. traverse . filtered (view (setObject_name . to isRealEdition))
rs & traverse_ (\x -> print x >> putStrLn "")






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

import Data.Fuzzy as X

--

import Control.Lens   as X hiding
  ((<&>))

--

import Control.Exception as X (evaluate)

import Prelude.Spiros as X hiding
 (Strict, index, at, snoc, uncons,Format)

----------------------------------------
