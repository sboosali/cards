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

validateQuery :: RawQuery -> Maybe ValidQuery
validateQuery = fromPredicate T.null

{- | for `fmapMaybe`.

-}
fromPredicate :: (a -> Bool) -> (a -> Maybe a)
fromPredicate p = \x -> if p x then Just x else Nothing

----------------------------------------

execQuery :: CardDatabase -> Query -> Results
execQuery db = parseQuery >>> runQuery db 

-- execQuery :: CardDatabase -> Query -> Text
-- execQuery db = parseQuery >>> runQuery db >>> formatResults 

parseQuery :: Text -> Query 
parseQuery q = T.toLower q

runQuery :: CardDatabase -> Query -> Results
runQuery db q = db & filter (\Card{..} -> q `T.isInfixOf` _cardName) 
 -- (_cardName == q) 
 -- & fmap _cardText
----------------------------------------

abbreviations :: Map Text Text
abbreviations =
  [ "cmc"-: "converted mana cost"
  , "kill"-: "destroy"
  ]

----------------------------------------