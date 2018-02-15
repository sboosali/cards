{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-| The query language(s) and search functionality. 

-}
module Cards.Frontend.Query where

import Cards.Frontend.Extra
import Cards.Frontend.Types
-- import Cards.Frontend.DB
-- import Cards.Frontend.Result

import qualified Data.Text as T

--import qualified Text.Megaparsec as P
-- 

----------------------------------------

{-| @magiccards.info@'s sytax

-}
data MCISyntax = MCISyntax
 { mciFreeText :: Maybe Text
 , mciFields   :: Map Text [Text]
 }

----------------------------------------

{-| @magiccards.info@'s behavior, features, predicates, etc.

-}
data MCIQuery

----------------------------------------

-- {-|

-- -}
-- data LogicSyntax

----------------------------------------

-- {-|

-- a @Prolog@-like language (simpler than / a subset thereof), with unification

-- -}
-- data LogicQuery

----------------------------------------

parseMCISyntax :: Text -> Maybe MCISyntax
parseMCISyntax t = Just $ MCISyntax (Just t) []

----------------------------------------

----------------------------------------

{- | non-empty and 'normalize'd (case insensitive, etc).

-}
validateQuery :: RawQuery -> Maybe ValidQuery
validateQuery
  = normalize
  > fromPredicate (T.null > not)
  > fmap ValidQuery

{- | for `fmapMaybe`.

-}
fromPredicate :: (a -> Bool) -> (a -> Maybe a)
fromPredicate p = \x -> if p x then Just x else Nothing

----------------------------------------

-- parseQuery :: Text -> RawQuery 
-- parseQuery 
--   = normalize 
-- --  > RawQuery

 -- (_cardName == q) 
 -- & fmap _cardText
----------------------------------------

abbreviations :: Map Text Text
abbreviations =
  [ "cmc"-: "converted mana cost"
  , "kill"-: "destroy"
  ]

----------------------------------------

normalize :: Text -> Text
normalize
  = T.toLower
  > T.strip

normalizeS :: String -> Text
normalizeS = T.pack > normalize

----------------------------------------
