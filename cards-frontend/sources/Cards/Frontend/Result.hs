{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-| The result formats/behavior, and rendering functionality. 

-}
module Cards.Frontend.Result where

import Cards.Frontend.Extra
import Cards.Frontend.Types

import qualified Data.Text as T

----------------------------------------

noResults :: Results
noResults = []

----------------------------------------

{-| TODO

-}
getImageUrl :: Card -> Text
getImageUrl Card{..} = _cardName

----------------------------------------

{-| TODO

pretty print a card.

-}
ppCard :: Card -> [Text]
ppCard Card{..} =
 [outerDivider, _cardName, innerDivider, _cardText]
 where
 outerDivider = T.replicate 40                   "="
 innerDivider = T.replicate (T.length _cardName) "-"

----------------------------------------

