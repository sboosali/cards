{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-| 

-}
module Cards.Syntax.Operators where

-- import qualified Text.Parsers.Frisby      as F
-- import qualified Text.Parsers.Frisby.Char as F
-- import           Text.Parsers.Frisby hiding (text, (<>))
-- import Enumerate
-- import Enumerate.Function

import qualified Data.Text.Lazy as T

import qualified Data.Set as Set  
-- import qualified Data.List.NonEmpty as NonEmpty
-- import qualified Data.Map as Map

import Prelude.Spiros hiding (P)
import Data.List (groupBy, sortOn)

----------------------------------------

newtype OperatorTable = OperatorTable
  { getOperatorTable :: [Text] }

instance IsList OperatorTable where
 type Item OperatorTable = Text
 toList   = getOperatorTable
 fromList = toOperatorTable

----------------------------------------

data Syntax = Syntax
 { unaryPrefix         :: [Text]
 , unaryPostfix        :: [Text]
 , binaryInfix         :: [Text]
 , multiaryAssociative :: [Text]
 , ternaryGrouping     :: [(Text,Text)]
 , quotationGrouping   :: [(Text,Text)]
 }


{-

arithmeticSyntax :: Syntax
arithmeticSyntax = Syntax{..}
 where
 unaryPrefix = ["-"]
 unaryPostfix = ["!"]
 binaryInfix = ["-", ""]
 multiaryAssociative = ["+", "<=", ...]
 ternaryGrouping =
  [ "(" -: ")"
  , both "|"
  ...
  ]
 quotationGrouping =
  [ both "\""
  , both "''"
  , "<<" -: ">>"
  ...
  ]

-}

----------------------------------------

{-|

descending order (i.e. longest first)

Like a topological sort the partial order on "longer".

-}
toposortOnLength :: [Text] -> [[Text]]
toposortOnLength 
 = groupBy ((==) `on` T.length)
 > sortOn (length > negate)


toOperatorTable :: [Text] -> OperatorTable 
toOperatorTable
 = filter (not . T.null)
 > toposortOnLength
 > concat
 > OperatorTable


operatorAlphabet :: OperatorTable -> Set Char
-- [Char]
operatorAlphabet 
 = getOperatorTable
 > T.concat
 > T.unpack
 > Set.fromList


isOperatorTableSymbolic :: OperatorTable -> Bool
isOperatorTableSymbolic
 = operatorAlphabet
 > Set.toList
 > all isSymbol




-- pOperator :: OperatorTable  -> P s Text
-- pOperator (OperatorTable os) = 
--  choice os

----------------------------------------
