
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-| The query language(s) and search functionality.

It's shared by most of the query syntaxes, and run against the database. 
(i.e. a user query in some syntax is just sugar for the query).

The @.Syntax._.Compiler@ modules convert some syntax into this query type.

-}
module Cards.Query.Types where

--import qualified Data.Text as T

----------------------------------------

data Query = Query
 {
 }

----------------------------------------
