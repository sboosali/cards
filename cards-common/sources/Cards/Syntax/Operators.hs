{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE RankNTypes #-}

{-| 

-}
module Cards.Syntax.Operators where

import Text.Earley
import Text.Earley.Mixfix

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

main :: IO ()
main = do
  putStrLn ""
  mixfixComparisons & printComparisons

-- mixfixMain :: IO ()
-- mixfixMain = do
--   mixfixExamples & traverse_ printMixfixExprReport

-- printMixfixExprReport ((actual, report), expected) = do
--   putStrLn ""
--   traverse_ print actual
--   print report
--   traverse_ print expected

printComparisons :: (Show a, Traversable t) => t (Comparison a) -> IO ()
printComparisons = traverse_ printComparison

printComparison :: Show a => Comparison a -> IO ()
printComparison (Comparison Identical Expectation{..}) = do
  putStrLn "----------------------------------------"
  print Identical
  putStrLn ""
  print expected
  putStrLn "" 
printComparison (Comparison Different expectation) = do
  putStrLn "----------------------------------------"
  print Different
  putStrLn ""
  print expectation
  putStrLn ""
  -- expected & print  {-traverse_-}
  -- actual   & print  {-traverse_-}

displayComparison :: Show a => Expectation a -> String
displayComparison = show > drop (length ("Expectation "::String))

-- (=?) :: Eq a => Expectation a -> (Compared, Expectation a)
-- (=?) x y = (x y, x y)
  
(=?) :: Eq a => a -> a -> Comparison a
(=?) actual expected = Comparison
  (actual `expecting` expected)
  Expectation{..}

expecting :: Eq a => a -> a -> Compared
expecting x y =
  if   x == y
  then Identical
  else Different
-- actual `expecting` expected

data Comparison a = Comparison Compared (Expectation a)

data Expectation a = Expectation
 { actual   :: a
 , expected :: a
 } deriving (Show)

data Compared 
  = Identical 
  | Different 
  deriving (Show)

-- mixfixExamples
 -- :: [ ( ([MixfixExpr], Report String [String])
 --      , [MixfixExpr]
 --      )
 --    ]

parses
  :: ()  -- (IsString s)
  => Parser e [String] a
  -> (String -> [String])
  -> String
  -> [a]
parses p tokenize s = fullParses p (tokenize s) & fst
  --where
  -- s = fromString t

mixfixComparisons :: [Comparison [MixfixExpr]]
mixfixComparisons = let x = Ident [Just "x"] in
  
  [ parses (parser mixfixGrammar) words "if x then x else x"
    =? [App ifthenelse [x, x, x]]

  , parses (parser mixfixGrammar) words "prefix x postfix"
    =? [App prefix [App postfix [x]]] 

  , parses (parser mixfixGrammar) words "x infix1 x infix2 x"
    =? [App infix1 [x, App infix2 [x, x]]] 

  , parses (parser mixfixGrammar) words "[ x ]"
    =? [App closed [x]] 

  , parses (parser mixfixGrammar) words "[x]"
    =? [App closed [x]] 

  , parses (parser mixfixGrammar) tokens "[ x ]"
    =? [App closed [x]] 

  , parses (parser mixfixGrammar) tokens "[x]"
    =? [App closed [x]] 

  ]

{-|

broadens 'Prelude.words'.

>>> words "[x]"
["[x]"]

>>> tokens "[x]"
["[","x","]"]

-}
tokens :: String -> [String]
tokens
  = groupBy areCharactersSimilar
  > fmap (filter (isSpace > not)) 
  > filter (null > not)

-- (isWhitespaceGeneralCategory > not))
  
areCharactersSimilar :: Char -> Char -> Bool
areCharactersSimilar 
  = ((==) `on` veryGeneralCategory)

  -- (==) `on` veryGeneralCategory

  -- areGeneralCategoriesSimilar `on` generalCategory

-- areCharacterCategoriesSimilar :: Char -> Char -> Bool
-- areCharacterCategoriesSimilar x y = 
--   where
--   c = generalCategory x
--   d = generalCategory y

-- areGeneralCategoriesSimilar
--   :: GeneralCategory -> GeneralCategory -> Bool
-- areGeneralCategoriesSimilar x y =

veryGeneralCategory :: Char -> Set GeneralCategory
veryGeneralCategory character
  = similarGeneralCategories
  & filter (Set.member category)
  & list2maybe -- take first
  & maybe Set.empty id -- default
  where
  category = generalCategory character

isWhitespaceGeneralCategory :: GeneralCategory -> Bool
isWhitespaceGeneralCategory = (Set.member &flip) whitespaceGeneralCategories

whitespaceGeneralCategories :: Set GeneralCategory
whitespaceGeneralCategories =
    [ Space
    , LineSeparator
    , ParagraphSeparator
    , Control
    ]

similarGeneralCategories :: [Set GeneralCategory]
similarGeneralCategories =
  [ [ UppercaseLetter
    , LowercaseLetter
    , TitlecaseLetter
    , ModifierLetter
    , OtherLetter
    ]

  , [ NonSpacingMark
    , SpacingCombiningMark
    , EnclosingMark
    ]  

  , [ DecimalNumber
    , LetterNumber
    , OtherNumber
    ]
    
  , [ ConnectorPunctuation
    , DashPunctuation
    , OpenPunctuation
    , ClosePunctuation
    , OtherPunctuation
    ]
    
  , [ InitialQuote
    , FinalQuote
    ]

  , [ MathSymbol
    , CurrencySymbol
    , ModifierSymbol
    , OtherSymbol
    ]
  
  , [ Space
    , LineSeparator
    , ParagraphSeparator
    , Control
    ]

  , [ Format
    ]
    
  , [ Surrogate
    ]
    
  , [ PrivateUse
    ]
    
  , [ NotAssigned
    ]

  ]    

{-


UppercaseLetter
LowercaseLetter
TitlecaseLetter
ModifierLetter
OtherLetter
NonSpacingMark
SpacingCombiningMark
EnclosingMark
DecimalNumber
LetterNumber
OtherNumber
ConnectorPunctuation
DashPunctuation
OpenPunctuation
ClosePunctuation
InitialQuote
FinalQuote
OtherPunctuation
MathSymbol
CurrencySymbol
ModifierSymbol
OtherSymbol
Space
LineSeparator
ParagraphSeparator
Control
Format
Surrogate
PrivateUse
NotAssigned


UppercaseLetter	
Lu: Letter, Uppercase

LowercaseLetter	
Ll: Letter, Lowercase

TitlecaseLetter	
Lt: Letter, Titlecase

ModifierLetter	
Lm: Letter, Modifier

OtherLetter	
Lo: Letter, Other

NonSpacingMark	
Mn: Mark, Non-Spacing

SpacingCombiningMark	
Mc: Mark, Spacing Combining

EnclosingMark	
Me: Mark, Enclosing

DecimalNumber	
Nd: Number, Decimal

LetterNumber	
Nl: Number, Letter

OtherNumber	
No: Number, Other

ConnectorPunctuation	
Pc: Punctuation, Connector

DashPunctuation	
Pd: Punctuation, Dash

OpenPunctuation	
Ps: Punctuation, Open

ClosePunctuation	
Pe: Punctuation, Close

InitialQuote	
Pi: Punctuation, Initial quote

FinalQuote	
Pf: Punctuation, Final quote

OtherPunctuation	
Po: Punctuation, Other

MathSymbol	
Sm: Symbol, Math

CurrencySymbol	
Sc: Symbol, Currency

ModifierSymbol	
Sk: Symbol, Modifier

OtherSymbol	
So: Symbol, Other

Space	
Zs: Separator, Space

LineSeparator	
Zl: Separator, Line

ParagraphSeparator	
Zp: Separator, Paragraph

Control	
Cc: Other, Control

Format	
Cf: Other, Format

Surrogate	
Cs: Other, Surrogate

PrivateUse	
Co: Other, Private Use

NotAssigned	
Cn: Other, Not Assigned

-}

data MixfixExpr
  = Ident (Holey String)
  | App (Holey String) [MixfixExpr]
  deriving (Eq, Show)

mixfixGrammar :: Grammar r (Prod r String String MixfixExpr)
mixfixGrammar = mixfixExpression table
                                 (Ident . pure . Just <$> namedToken "x")
                                 App
  where
    hident = map (fmap token)
    table =
      [ [(hident ifthenelse, RightAssoc)]
      , [(hident prefix, RightAssoc)]
      , [(hident postfix, LeftAssoc)]
      , [(hident infix1, LeftAssoc)]
      , [(hident infix2, RightAssoc)]
      , [(hident closed, NonAssoc)]
      ]

ifthenelse, prefix, postfix, infix1, infix2, closed :: Holey String
ifthenelse = [Just "if", Nothing, Just "then", Nothing, Just "else", Nothing]
prefix = [Just "prefix", Nothing]
postfix = [Nothing, Just "postfix"]
infix1 = [Nothing, Just "infix1", Nothing]
infix2 = [Nothing, Just "infix2", Nothing]
closed = [Just "[", Nothing, Just "]"]


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
