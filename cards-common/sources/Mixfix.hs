{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|

also see <https://hackage.haskell.org/package/Earley-0.12.0.1/docs/Text-Earley-Mixfix.html>

-}
module Mixfix where

import qualified Data.Set as Set  
import qualified Data.Text.Lazy as T
import Data.List (sortOn)

import Enumerate (Enumerable)
import Prelude.Spiros

----------------------------------------

{-|

-}
data Mixfix a = Mixfix
 { primitive :: a
 , operators :: Operators a
 } deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable)

----------------------------------------

{-|


-}
newtype Operators a = Operators
  { getOperators :: [[Operator a]]
  } deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable)

-- instance IsList (Operators a) where
--  type Item (Operators a) = [Operator a]
--  toList   = getOperators
--  fromList = toOperators



----------------------------------------

{-|


e.g. "if_then_else_":

@
Operator 'def' [Just "if", Nothing, Just "then", Nothing, Just "else", Nothing]

-- :: 'Operator' String
@

-}
data Operator a = Operator
  { associativity :: Associativity
  , holes         :: Holey a
  } deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable)

-- instance IsList Operator a where
--  type Item (Operator a) = Maybe a
-- NO Associativity

----------------------------------------

{-|

-}
data Associativity
  = LeftAssociative
  | NonAssociative
  | RightAssociative
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

instance Default Associativity where def = NonAssociative

----------------------------------------

{-|

-}

-- | An identifier with identifier parts ('Just's), and holes ('Nothing's)
-- representing the positions of its arguments.
--
-- Example (commonly written "if_then_else_"):
-- @['Just' "if", 'Nothing', 'Just' "then", 'Nothing', 'Just' "else", 'Nothing'] :: 'Holey' 'String'@
type Holey a = [Maybe a]

----------------------------------------

newtype OperatorTable = OperatorTable
  { getOperatorTable :: [Text]
  } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

instance IsList OperatorTable where
 type Item OperatorTable = Text
 toList   = getOperatorTable
 fromList = toOperatorTable

{-|

Sorts in descending order (i.e. longest first)

Like a topological sort the partial order on "longer".

Used to support operators that overlap with or subsume each other (e.g. "<" is a strict prefix-string of "<=").  

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

----------------------------------------

data Syntax = Syntax
 { unaryPrefix         :: [Text]
 , unaryPostfix        :: [Text]
 , binaryInfix         :: [Text]
 , multiaryAssociative :: [Text]
 , ternaryGrouping     :: [(Text,Text)]
-- , ternaryQuotation    :: [(Text,Text)]
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
 ternaryQuotation =
  [ both "\""
  , both "''"
  , "<<" -: ">>"
  ...
  ]

-}

----------------------------------------
