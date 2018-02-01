{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-| The (in-memory, somewhat indexed) database of cards.

-}
module Cards.Frontend.DB where

import Cards.Frontend.Extra
import Cards.Frontend.Types

--import qualified Data.Text as T

----------------------------------------

defaultCardDatabase :: CardDatabase
defaultCardDatabase = fmap (Card & uncurry)
 [ "fire" -: "deal 3 damage", "ice" -: "tap target permanent", "kavu firemaw" -: "FIREMAW" ]

----------------------------------------
