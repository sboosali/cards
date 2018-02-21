
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-| 


-}
module Cards.Syntax.MagicCardsInfo.Parser where

import Cards.Syntax.MagicCardsInfo.Types

import Cards.Query.Types

-- import           Text.Megaparsec ()
-- import qualified Text.Megaparsec as P

import Text.Parsers.Frisby

import Prelude.Spiros

----------------------------------------

freeform :: Text -> Syntax
freeform t = Syntax mciFreeText mciFields
 where
 mciFreeText = Just t
 mciFields   = []

----------------------------------------

prefixOperators :: [Text]
prefixOperators =
  [ "!", "-", "not" ]

infixOperators :: [Text]
infixOperators =
  [ ":", "!", "=", "<", ">", "<=", ">=", "or", "and" ]

invalidNakedChars :: [Char]
invalidNakedChars =
  " :!\"()"


----------------------------------------
