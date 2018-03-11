{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE RankNTypes #-}

--{-# LANGUAGE DeriveAnyClass #-}

{-| 

-}
module Operators where

--import Tokenize
  
import Text.Earley
import Text.Earley.Mixfix

-- import qualified Text.Parsers.Frisby      as F
-- import qualified Text.Parsers.Frisby.Char as F
-- import           Text.Parsers.Frisby hiding (text, (<>))

--import Enumerate (Enumerable)
-- import Enumerate.Function

import qualified Data.Set as Set  
-- import qualified Data.List.NonEmpty as NonEmpty
-- import qualified Data.Map as Map

import Prelude.Spiros hiding (P)
import Data.List (groupBy)
import Data.Char (GeneralCategory(..))

----------------------------------------

----------------------------------------


-- pOperator :: OperatorTable  -> P s Text
-- pOperator (OperatorTable os) = 
--  choice os

----------------------------------------



----------------------------------------

mainOperators :: IO ()
mainOperators = do
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
parses p tokenize' s = fullParses p (tokenize' s) & fst
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

-}
tokens :: String -> [String]
tokens
  = groupBy areCharactersSimilar
  > filter (null > not)

{-|

broadens 'Prelude.words'.

>>> words "[x]"
["[x]"]

>>> tokensNoWhitespace "[x]"
["[","x","]"]

NOTE:

>>> generalCategory <$> ([':', '"'] :: [Char])
[OtherPunctuation,OtherPunctuation]

-}
tokensNoWhitespace :: String -> [String]
tokensNoWhitespace
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

{-




NOTE:

>> ([8..127] <&> chr <&> (generalCategory &&& (:[]))) & Map.fromListWith (++) & Map.traverseWithKey (\category characters -> (do; print category; traverse_ print characters; putStrLn ""))

UppercaseLetter
'Z'
'Y'
'X'
'W'
'V'
'U'
'T'
'S'
'R'
'Q'
'P'
'O'
'N'
'M'
'L'
'K'
'J'
'I'
'H'
'G'
'F'
'E'
'D'
'C'
'B'
'A'

LowercaseLetter
'z'
'y'
'x'
'w'
'v'
'u'
't'
's'
'r'
'q'
'p'
'o'
'n'
'm'
'l'
'k'
'j'
'i'
'h'
'g'
'f'
'e'
'd'
'c'
'b'
'a'

DecimalNumber
'9'
'8'
'7'
'6'
'5'
'4'
'3'
'2'
'1'
'0'

ConnectorPunctuation
'_'

DashPunctuation
'-'

OpenPunctuation
'{'
'['
'('

ClosePunctuation
'}'
']'
')'

OtherPunctuation
'\\'
'@'
'?'
';'
':'
'/'
'.'
','
'*'
'\''
'&'
'%'
'#'
'"'
'!'

MathSymbol
'~'
'|'
'>'
'='
'<'
'+'

CurrencySymbol
'$'

ModifierSymbol
'`'
'^'

Space
' '

Control
'\DEL'
'\US'
'\RS'
'\GS'
'\FS'
'\ESC'
'\SUB'
'\EM'
'\CAN'
'\ETB'
'\SYN'
'\NAK'
'\DC4'
'\DC3'
'\DC2'
'\DC1'
'\DLE'
'\SI'
'\SO'
'\r'
'\f'
'\v'
'\n'
'\t'
'\b'

-}







{-


fromList [(UppercaseLetter,"ZYXWVUTSRQPONMLKJIHGFEDCBA"),(LowercaseLetter,"zyxwvutsrqponmlkjihgfedcba"),(DecimalNumber,"9876543210"),(ConnectorPunctuation,"_"),(DashPunctuation,"-"),(OpenPunctuation,"{[("),(ClosePunctuation,"}])"),(OtherPunctuation,"\\@?;:/.,*'&%#\"!"),(MathSymbol,"~|>=<+"),(CurrencySymbol,"$"),(ModifierSymbol,"`^"),(Space," "),(Control,"\DEL\US\RS\GS\FS\ESC\SUB\EM\CAN\ETB\SYN\NAK\DC4\DC3\DC2\DC1\DLE\SI\SO\r\f\v\n\t\b")]
*Cards.Syntax.Operators Data.List Map>

([8..127] <&> chr <&> (generalCategory &&& id)) & fromListWith (++) 

>>> ascii = chr <$> 
>>> asciiCategories = (id &&& generalCategory) <$> ascii
>>> asciiCategorized = asciiCategories & foldr (\(character, category) -> Map.insert character category)
>>> asciiCategorized & traverse_ 

>>> asciiCategories & traverse_ (\(character, category) -> do putStrLn""; print character; print category)


'\b'
Control

'\t'
Control

'\n'
Control

'\v'
Control

'\f'
Control

'\r'
Control

'\SO'
Control

'\SI'
Control

'\DLE'
Control

'\DC1'
Control

'\DC2'
Control

'\DC3'
Control

'\DC4'
Control

'\NAK'
Control

'\SYN'
Control

'\ETB'
Control

'\CAN'
Control

'\EM'
Control

'\SUB'
Control

'\ESC'
Control

'\FS'
Control

'\GS'
Control

'\RS'
Control

'\US'
Control

' '
Space

'!'
OtherPunctuation

'"'
OtherPunctuation

'#'
OtherPunctuation

'$'
CurrencySymbol

'%'
OtherPunctuation

'&'
OtherPunctuation

'\''
OtherPunctuation

'('
OpenPunctuation

')'
ClosePunctuation

'*'
OtherPunctuation

'+'
MathSymbol

','
OtherPunctuation

'-'
DashPunctuation

'.'
OtherPunctuation

'/'
OtherPunctuation

'0'
DecimalNumber

'1'
DecimalNumber

'2'
DecimalNumber

'3'
DecimalNumber

'4'
DecimalNumber

'5'
DecimalNumber

'6'
DecimalNumber

'7'
DecimalNumber

'8'
DecimalNumber

'9'
DecimalNumber

':'
OtherPunctuation

';'
OtherPunctuation

'<'
MathSymbol

'='
MathSymbol

'>'
MathSymbol

'?'
OtherPunctuation

'@'
OtherPunctuation

'A'
UppercaseLetter

'B'
UppercaseLetter

'C'
UppercaseLetter

'D'
UppercaseLetter

'E'
UppercaseLetter

'F'
UppercaseLetter

'G'
UppercaseLetter

'H'
UppercaseLetter

'I'
UppercaseLetter

'J'
UppercaseLetter

'K'
UppercaseLetter

'L'
UppercaseLetter

'M'
UppercaseLetter

'N'
UppercaseLetter

'O'
UppercaseLetter

'P'
UppercaseLetter

'Q'
UppercaseLetter

'R'
UppercaseLetter

'S'
UppercaseLetter

'T'
UppercaseLetter

'U'
UppercaseLetter

'V'
UppercaseLetter

'W'
UppercaseLetter

'X'
UppercaseLetter

'Y'
UppercaseLetter

'Z'
UppercaseLetter

'['
OpenPunctuation

'\\'
OtherPunctuation

']'
ClosePunctuation

'^'
ModifierSymbol

'_'
ConnectorPunctuation

'`'
ModifierSymbol

'a'
LowercaseLetter

'b'
LowercaseLetter

'c'
LowercaseLetter

'd'
LowercaseLetter

'e'
LowercaseLetter

'f'
LowercaseLetter

'g'
LowercaseLetter

'h'
LowercaseLetter

'i'
LowercaseLetter

'j'
LowercaseLetter

'k'
LowercaseLetter

'l'
LowercaseLetter

'm'
LowercaseLetter

'n'
LowercaseLetter

'o'
LowercaseLetter

'p'
LowercaseLetter

'q'
LowercaseLetter

'r'
LowercaseLetter

's'
LowercaseLetter

't'
LowercaseLetter

'u'
LowercaseLetter

'v'
LowercaseLetter

'w'
LowercaseLetter

'x'
LowercaseLetter

'y'
LowercaseLetter

'z'
LowercaseLetter

'{'
OpenPunctuation

'|'
MathSymbol

'}'
ClosePunctuation

'~'
MathSymbol

'\DEL'
Control


-}
