
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-| 

-}
module Cards.Syntax.MagicCardsInfo.Compiler where

import Cards.Syntax.MagicCardsInfo.Types
--import Cards.Syntax.MagicCardsInfo.Validator 

import Cards.Query.Types

----------------------------------------

compile :: Syntax i j -> Query
compile _ = Query

----------------------------------------
