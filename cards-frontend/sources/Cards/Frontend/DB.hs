{-# LANGUAGE NoImplicitPrelude #-}

--{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE OverloadedLists #-}

{-| The (in-memory, somewhat indexed) database of cards.

-}
module Cards.Frontend.DB where

import qualified Cards.AllCards as AllCards

import Cards.Frontend.Extra
import Cards.Frontend.Types

import qualified Data.Text as T

----------------------------------------

defaultCardDatabase :: CardDatabase
defaultCardDatabase = AllCards.cards
 & fmap (\(x,y) -> Card (T.pack x) (T.pack y))

 -- [ "fire" -: "deal 3 damage", "ice" -: "tap target permanent", "kavu firemaw" -: "FIREMAW" ]

----------------------------------------

{-


defaultCardDatabase :: CardDatabase
defaultCardDatabase = AllCards.cards
 & fmap (\(x,y) -> (T.pack x, T.pack y))
 & Map.fromList
 & fmap (Card & uncurry)

 -- [ "fire" -: "deal 3 dama

-}