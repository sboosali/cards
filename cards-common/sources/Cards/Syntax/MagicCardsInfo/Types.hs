
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-| 

-}
module Cards.Syntax.MagicCardsInfo.Types where

import Prelude.Spiros

----------------------------------------

{-| @magiccards.info@'s sytax

-}
data Syntax = Syntax
 { mciFreeText :: Maybe Text
 , mciFields   :: Map Text [Text]
 }

----------------------------------------

{-| @magiccards.info@'s behavior, features, predicates, etc.

-}

----------------------------------------


freeform :: Text -> Syntax
freeform t = Syntax mciFreeText mciFields
 where
 mciFreeText = Just t
 mciFields   = []

----------------------------------------
