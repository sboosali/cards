{-# LANGUAGE NoImplicitPrelude #-}

--{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE OverloadedLists #-}

{-| The query language(s) and search functionality. 

-}
module Cards.Frontend.Search where

--import Cards.Frontend.Extra
import Cards.Frontend.Types
import Cards.Frontend.Query

import qualified Data.Text as T

import Prelude.Spiros

----------------------------------------

execQuery :: CardDatabase -> RawQuery -> Maybe Results
execQuery db
  = validateQuery
  > fmap (runQuery db)

-- execQuery :: CardDatabase -> Query -> Text
-- execQuery db = parseQuery >>> runQuery db >>> formatResults 

runQuery :: CardDatabase -> ValidQuery -> Results
runQuery (CardDatabase db) (ValidQuery q)
  = db
  & filter (\Card{..} -> q `T.isInfixOf` _cardName)
  & fmap Result
  & Results

----------------------------------------
