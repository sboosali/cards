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

import Memoize

import Prelude.Spiros

----------------------------------------

execQuery :: CardDatabase -> RawQuery -> Maybe Results
execQuery db
  = validateQuery
  > fmap (runQuery db)

-- execQuery :: CardDatabase -> Query -> Text
-- execQuery db = parseQuery >>> runQuery db >>> formatResults 

runQuery :: CardDatabase -> ValidQuery -> Results
runQuery database = memoWithMap go
  where
  go = runQuery' database
{-# INLINE runQuery #-}  
--TODO caching; close over the CardDatabase, which changes infrequently anyways. 

runQuery' :: CardDatabase -> ValidQuery -> Results
runQuery' (CardDatabase db) = \query
 -> db
  & filter (matchCard query)
  & fmap Result
  & Results
{-# INLINE runQuery' #-}  

matchCard :: ValidQuery -> Card -> Bool --TODO Maybe Result
matchCard (ValidQuery q) Card{..} =
  q `T.isInfixOf` _cardName

----------------------------------------
