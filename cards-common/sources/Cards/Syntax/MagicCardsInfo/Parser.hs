
{-# LANGUAGE CPP #-}

#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif

{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}

{-| 

e.g.

@

Birds of Paradise
"Birds of Paradise"

!Anger
-- Match the full name

o:Flying
o:"First strike"
o:{T} o:"add one mana of any color"

o:"whenever ~ deals combat damage"
-- `~` is an alias for the card name

t:angel
t:"legendary angel"
t:basic
t:"arcane instant"

c:w
-- Any card that is white

c:wu
-- Any card that is white or blue

c:wum
-- Any card that is white or blue, and multicolored

c!w
-- Cards that are only white

c!wu
-- Cards that are only white or blue, or both

c!wum
-- Cards that are only white and blue, and multicolored

c!wubrgm
-- Cards that are all five colors

c:m
-- Any multicolored card

c:l or c:c
-- Lands and colorless cards


ci:wu
-- Any card that is white or blue, but does not contain any black, red or green mana symbols

in:wu
-- Any card that is white or blue according to the color indicator.

mana=3G
-- Spells that cost exactly 3G, or split cards that can be cast with 3G

mana>=2WW
-- Spells that cost at least two white and two colorless mana

mana<GGGGGG
-- Spells that can be cast with strictly less than six green mana

mana>=2RR mana<=6RR
-- Spells that cost two red mana and between two and six colorless mana

mana>={2/R}
mana>={W/U}
mana>={UP}

pow<=1
tou<pow
pow>=cmc
cmc=7
cmc>=*

r:common
r:uncommon
r:rare
r:mythic

f:standard
f:block
f:extended
f:vintage
f:classic
f:legacy
f:modern
f:commander

banned:legacy
legal:standard
restricted:vintage

a:"Quinton Hoover"
a:Guay

e:al/en
-- Uses the abbreviations that are listed on the sitemap

e:al,be
-- Cards that appear in Alpha or Beta

e:al+be
-- Cards that appear in Alpha and Beta

e:al,be -e:al+be
-- Cards that appear in Alpha or Beta but not in both editions

year<=1995
-- Cards printed in 1995 and earlier

is:split
is:flip
is:vanilla
-- Creatures with no card text

is:old
is:new
is:future
-- Old/new/future card face

is:timeshifted
is:funny
not:funny
-- Unglued/Unhinged/Happy Holidays Promos

is:promo
-- Promotional cards

is:promo is:old
-- Promotional cards with the original card face

is:permanent
is:spell

is:black-bordered
is:white-bordered
is:silver-bordered

has:foil

l:de
l:it
l:jp

o:"whenever ~" ((o:"deals damage to a" or o:"deals combat damage to a") (o:opponent or o:player)) or o:"attacks and isn't blocked")

@

e.g.

@
_ and _
_ _
-- i.e. whitespace

_ or _

not _
- _

( _ )
@


-}
module Cards.Syntax.MagicCardsInfo.Parser where

import Cards.Syntax.Extra
import Cards.Syntax.MagicCardsInfo.Types
import Cards.Syntax.MagicCardsInfo.Static
import Cards.Syntax.MagicCardsInfo.Known 
import Cards.Syntax.MagicCardsInfo.Printer

--import Cards.Query.Types

-- import           Text.Megaparsec ()
-- import qualified Text.Megaparsec as P

--import qualified Text.Parsers.Frisby as Frisby
import qualified Text.Parsers.Frisby.Char as F
import           Text.Parsers.Frisby hiding (text, (<>))

--import Data.Thyme (Day)

import Data.Thyme.Calendar (YearMonthDay(..))

import Enumerate
-- import Enumerate.Function

-- import qualified Data.Text.Lazy as T

-- import qualified Data.List.NonEmpty as NonEmpty
--import qualified Data.Map as Map

import Prelude.Spiros hiding (P)
import Prelude (error)

----------------------------------------

{-|

>> mci "!Anger"
Right (ExactQuery_ "Anger")

>> mci "o:Flying"
Right (StatementQuery_ (Statements {_statementFreeform = [], _statementAttributes = Attributes {getAttributes = [Attribute {subject = "o", verb = ":", object = TextAttribute "Flying"}]}}))

>> mci "o:\"First strike\""

>> mci "year<=95"
Right (StatementQuery_ (Statements {_statementFreeform = [], _statementAttributes = Attributes {getAttributes = [Attribute {subject = "year", verb = "<=", object = DateAttribute (YearMonthDay {ymdYear = 1995, ymdMonth = 0, ymdDay = 0})}]}}))

>> mci "e:al/en,be"

>> mci "e:al/en,be -e:al+be"


-}
mci :: String -> Either SyntaxError KnownQuery_
mci = runMagicCardsInfo' > fst > maybe l Right
  where
  l = Left (SyntaxError "")
  -- maybe2either

{-|


-}
parseMagicCardsInfo :: String -> Complete (KnownQuery_)
parseMagicCardsInfo = runMagicCardsInfo'

----------------------------------------

runMagicCardsInfo'
  :: (ParseableNumeric i, ParseableNumeric j)
  => String
  -> (Complete (Query_ i j))
runMagicCardsInfo' = runPeg gMagicCardsInfo' 

gMagicCardsInfo'
  :: (ParseableNumeric i, ParseableNumeric j)
  => PM s (P s (Complete (Query_ i j)))
gMagicCardsInfo' = complete gMagicCardsInfo

{-|


-}
runMagicCardsInfo
  :: (ParseableNumeric i, ParseableNumeric j)
  => String
  -> (Query_ i j)
runMagicCardsInfo = runPeg gMagicCardsInfo 

---------------------------------------

--NOTE leading or trailing whitespace, whether around the whole query or between attributes, never matters. 

{-|

e.g.

@
-- a:"Quinton Hoover"
Statement "a" ":" (Text' "Quinton Hoover")

-- e:al/en
Statement "e" ":" (Separated "/" ["al", "en"])

-- e:al/en,be -e:al+be
Separated " "
 [ Statement "e" ":"
   ( Separated ","
     [ Separated "/" ["al", "en"]
     , "be"
     ])
 , Prefixed "-"
     ( Statement "e" ":"
        ( Separated "+" ["al", "be"]))
 ]

-- year<=1995
Statement "year" "<=" (Date' 1995)
@

i.e.: we don't interpret the meaning of anything, like the string "<=", but we do: (1) parse into simple types, like strings, dates, or some enums (like color); and (2), group substrings via the associativity / precedence of the operators, both alphabetic and symbolic, like the unary prefixes "-" and "not", or distinguishing @"/"@ from @","@ in @"al/en,be"@ (i.e., as an sexp, @(("al" "/" "en") "," "be")", including pseudo-operators ("whitepsace juxtaposition" and parentheses). 



-}
gMagicCardsInfo
  :: ( ParseableNumeric i
     , ParseableNumeric j
     )
  => G s (Query_ i j)
gMagicCardsInfo = mdo
  let gExactName = rule$ preSpaceable pExactName <&> ExactQuery_
  
  statements' <- gStatements 
  exact       <- gExactName
  
  let statements = statements' <&> StatementQuery_
  
  p <- rule$ exact // statements
  q <- rule$ postSpaceable p 
  return q

  -- pStatements <- gStatements <&> StatementQuery_ 
  -- pExact      <- gExactName  <&> ExactQuery_ 

----------------------------------------

pExactName :: P s Text
pExactName = p <&> toS
  where
  p = char '!' *> rest

gStatements
  :: forall i j s.
     ( ParseableNumeric i
     , ParseableNumeric j
     )
  => G s (Statements i j)
gStatements = mdo

  

  psTextAttributes      <- textRule        ["o","t","a"] 

  {- ^ NOTE magiccards.info itself doesn't support equality,
       e.g. `o!Flying`
  -}

  psNumericAttributes   <- rule$ pN

  psManaAttributes      <- rule$ manaAttribute        ["mana"] 

  psColorAttributes     <- rule$ chromaticAttribute   ["c","ci","in"]

  psDateAttributes      <- rule$ dateAttribute        ["year"]

  psOrderedAttributes   <- rule$ orderedEnumAttribute ["e","r"]

  psUnorderedAttributes <- rule$ unorderedEnumAttribute
      ["l"
      ,"f"
      ,"is", "not","has" 
      ,"banned", "legal", "restricted"
      ]

  pAttribute <- rule$ choice
      [ psTextAttributes     
      , psNumericAttributes
      , psManaAttributes
      , psColorAttributes
      , psDateAttributes
      , psOrderedAttributes
      , psUnorderedAttributes
      ]

  pAttributes <- rule$ many1 pAttribute <&> Attributes

  pFreeform <- rule$ pure []

  p <- rule$ Statements <$> pFreeform <*> pAttributes
  return p

  where
  pN :: P s (Attribute i j)
  pN = numericAttribute ["cmc","pow","tou"] 







 --  psText      <- rule$ mciTextAttributes
 --  psNumeric   <- rule$ mciNumericAttributes 
 --  psMana      <- rule$ mciManaAttributes 
 --  psColor     <- rule$ mciColorAttributes 
 --  psDate      <- rule$ mciDateAttributes 
 --  psUnordered <- rule$ mciUnorderedAttributes

 --  p           <- rule$ _
 --  return p
  
 -- -- numbers / dates?
 -- -- p <- rule $ pure emptyQuery_

----------------------------------------
-- mci parsers

-- mciTextAttributes :: P s (Attribute i j)
-- mciTextAttributes = textAttribute ["o","t","a"] 
-- -- TODO magiccards.info doesn't support equality, e.g. `o!Flying`

-- mciNumericAttributes :: (Num i) => P s (Attribute i j)
-- mciNumericAttributes = numericAttribute ["cmc","pow","tou"] 

-- mciManaAttributes :: (Num j, Show j, Enumerable j) => P s (Attribute i j)
-- mciManaAttributes = manaAttribute ["mana"] 

-- mciColorAttributes :: P s (Attribute i j)
-- mciColorAttributes = chromaticAttribute ["c","ci","in"]

-- mciDateAttributes :: P s (Attribute i j)
-- mciDateAttributes = dateAttribute ["year"]

-- mciOrderedAttributes :: P s (Attribute i j)
-- mciOrderedAttributes = orderedEnumAttribute
--  ["e"
--  ,"r"
--  ]

-- mciUnorderedAttributes :: P s (Attribute i j)
-- mciUnorderedAttributes = unorderedEnumAttribute
--  ["l"
--   ,"f"
--  ,"is", "not","has" 
--  ,"banned", "legal", "restricted"
--  ]

----------------------------------------

-- {-|

-- -}  
-- textAttributes :: [[Text]] -> P s Attribute
-- textAttributes keywords = attribute keywords genericOperators_ pText

----------------------------------------

{-|

Parses a keyword (or a set of aliases for that keyword),
with the given separator (or, variations for the separator),
and the given parser.

e.g.

@
cmc:3
@

is parsed by

@
attribute ["cmc", ...] [":", ...] 'pNumeric'
-- where number :: P s Numeric
@


-}
attribute
  :: [Text]
  -> [Text]
  -> QuotableParser s (KnownAttribute i j) 
  -- -> P s (KnownAttribute i j)
  -- -> P s (KnownAttribute i j)
  -> P s (Attribute i j)
attribute keywords seperators ps = q
  where
  -- "subject / verb / object"
  s = texts keywords
  v = texts seperators
  o = quotable ps  -- quotable _pUnquoted _pQuoted 
  p = Attribute <$> s <*> v <*> o
  q = preSpaceable p

{-|

wraps 'attribute' (as 'quotable'' wraps 'quotable'). 

-}
attribute'
  :: [Text]
  -> [Text]
  -> P s (KnownAttribute i j)
  -> P s (Attribute      i j)
attribute' ks os p = attribute ks os (singletonQuotableParser p)

-- {-|


-- -}  
-- enumAttribute :: [Text] -> [Text] -> [Text] -> P s (Attribute i j)
-- enumAttribute ks os vs = attribute ks os
--   (textAttributes vs)

----------------------------------------

-- {-|


-- -}  
-- textAttributes :: [Text] -> P s (KnownAttribute i j)
-- textAttributes vs =
--   TextAttribute <$> (texts vs)

{-|

e.g.

@
t:"arcane instant"
@

is parsed by

@
textAttribute ["t", ...]
@

aka

@
attribute ["t", ...] [":", "!"] 'pText'
@

-}
textRule :: [Text] -> G s (Attribute i j)
textRule ks = do
  pQuotableText <- gQuotableText
  let p = TextAttribute <$> pQuotableText
  q <- rule$ attribute ks genericOperators_ p
  return q

gQuotableText :: G s Text
gQuotableText = do
  pQuoteable <- gQuotable pNakedText pQuotedText
  return pQuoteable

----------------------------------------

{-|


-}  
orderedEnumAttribute :: [Text] -> P s (Attribute i j)
orderedEnumAttribute ks = TextAttribute <$> 
  attribute ks numericOperators_ qEnum

{-|

-}  
unorderedEnumAttribute :: [Text] -> P s (Attribute i j)
unorderedEnumAttribute ks = TextAttribute <$> 
  attribute ks genericOperators_ qEnum

{-|

-}  
equatableEnumAttribute :: [Text] -> P s (Attribute i j)
equatableEnumAttribute ks = TextAttribute <$> 
  attribute ks [":"] qEnum

{-|

-}  
chromaticAttribute :: [Text] -> P s (Attribute i j)
chromaticAttribute ks = ChromaticAttribute <$> 
  attribute ks numericOperators_ qChromatic

{-|

-}
dateAttribute :: [Text] -> P s (Attribute i j)  
dateAttribute ks = DateAttribute <$> 
  attribute ks numericOperators_ qDate

{-|


-}  
numericAttribute :: (Num i) => [Text] -> P s (Attribute i j)
numericAttribute ks = NumericAttribute <$> 
  attribute ks numericOperators_ qNumeric

{-|

-} 
manaAttribute :: (ParseableNumeric j) => [Text] -> P s (Attribute i j)
manaAttribute ks = ManaAttribute <$> 
  attribute ks numericOperators_ qMana


----------------------------------------
  
equalityOperators_ :: [Text]
equalityOperators_ =
  [ ":"
  ]

genericOperators_ :: [Text]
genericOperators_ =
  [ ":"
  , "!"
  ]

numericOperators_ :: [Text]
numericOperators_ =
  [ ":"
  , "!"
  , "="
  , "<"
  , ">"
  , "<="
  , ">="
  ]

-- attribute :: [Text] -> SyntaxTable a -> P s Text -> P s Attribute
-- attribute keywords seperators pConstraint = p
--   where
--   v = quotable pConstraint
--   o = vocabulary seperators
--   k = texts keywords
--   p = Attribute <$> k <*> o <*> v

-- pAttribute :: [Text] -> P s Text -> P s Attribute
-- pAttribute ks pConstraint = do
--   v <- pQuotable pConstraint
--   k <- rule$ texts ks
--   p <- rule$
--     Attribute <$> (k <* char ':') <*> v
--   return p


----------------------------------------  

{-

pOracle :: P s Attribute
pOracle = textAttribute ["o"] 
--pOracle = attribute ["o"] genericOperators_ pText
-- TODO magiccards.info doesn't support equality, e.g. `o!Flying`

pTypes :: P s Attribute
pTypes = textAttribute ["t"] 
--pTypes = attribute ["t"] genericOperators_ pText

pArtist :: P s Attribute
pArtist = textAttribute ["a"] 

----------------------------------------

pCMC :: P s Attribute
pCMC = numericAttribute ["cmc"] 

pPower :: P s Attribute
pPower = numericAttribute ["pow"]

pToughness :: P s Attribute
pToughness = numericAttribute ["tou"] 

----------------------------------------

pYear :: P s Attribute
pYear = attribute ["year"] numericOperators_ pDate

pMana :: P s Attribute
pMana = attribute ["mana"] 

----------------------------------------

pColor :: P s Attribute
pColor = colorAttribute ["c"] []

pColorIdentity :: P s Attribute
pColorIdentity = colorAttribute ["ci"] []

pColorIndication :: P s Attribute
pColorIndication = colorAttribute ["in"]  []

----------------------------------------
  
pEdition :: P s Attribute
pEdition = enumAttribute ["e"] []

pFormat :: P s Attribute
pFormat = enumAttribute ["f"]  []
  
pRarity :: P s Attribute
pRarity = enumAttribute ["r"]  []

pLegality :: P s Attribute
pLegality = enumAttribute ["banned", "legal", "restricted"] []

pLanguage :: P s Attribute
pLanguage = enumAttribute ["l"]  []

pIs :: P s Attribute
--pIs :: Grammar Attribute --gIdentity
pIs = enumAttribute ["is", "not"] [":"] identityKeywords

pHas :: P s Attribute
pHas = enumAttribute ["has"] [":"] possessionKeywords

-}

----------------------------------------

-- qTextObject :: QuotableParser s (KnownAttribute i j)
-- qTextObject = qText <&> TextAttribute

-- qEnumObject :: QuotableParser s (KnownAttribute i j)
-- qEnumObject = qEnum <&> TextAttribute

-- qNumericObject :: (Num i) => QuotableParser s (KnownAttribute i j)
-- qNumericObject = qNumeric <&> NumericAttribute

-- qManaObject :: (ParseableNumeric j) => QuotableParser s (KnownAttribute i j)
-- qManaObject = qMana <&> ManaAttribute

-- qChromaticObject :: QuotableParser s (KnownAttribute i j)
-- qChromaticObject = qChromatic <&> ChromaticAttribute

-- qDateObject :: QuotableParser s (KnownAttribute i j)
-- --pDateObject :: (Show i) => QuotableParser s (KnownAttribute i j)
-- qDateObject = qDate <&> DateAttribute

----------------------------------------

-- qText :: QuotableParser s Text
-- qText = QuotableParser pNakedText pQuotedText

qEnum :: QuotableParser s Text
qEnum = singletonQuotableParser pToken

qChromatic :: QuotableParser s Chromatic
qChromatic = singletonQuotableParser pChromatic

qDate :: QuotableParser s Date
qDate = singletonQuotableParser pDate

qNumeric :: (Num i) => QuotableParser s (Numeric i)
qNumeric = singletonQuotableParser pNumeric

qMana :: (ParseableNumeric j) => QuotableParser s (ManaCost j)  
qMana = singletonQuotableParser pMana
--pMana :: (Show j) => P s (ManaCost j)

-- pGenericOperators :: P s GenericComparator
-- pGenericOperators = aliases genericOperators

-- pNumericOperators :: P s NumericComparator
-- pNumericOperators = aliases numericOperators

-- pNumericComparison :: P s Attribute
-- pNumericComparison = _ -- pAttribute' pNumeric

----------------------------------------

textUntil :: [Char] -> G s Text
textUntil cs = do
  pBad  <- rule$ oneOf cs
  pGood <- anyChar & manyUntil pBad 
  let p = pGood <&> toS
  return p
 -- where

----------------------------------------  

gQuotedText :: G s Text
gQuotedText = textUntil invalidQuotedChars

invalidQuotedChars :: [Char]
invalidQuotedChars =
  "\""
  -- TODO for nested quotations, e.g. in Llanowar Mentor, use two single quotes (i.e. '')

gNakedText :: G s Text
gNakedText = textUntil invalidNakedChars

invalidNakedChars :: [Char]
invalidNakedChars =
  " :!\"()"

-- pQuotedText :: P s Text
-- pQuotedText = many1 pQuotedChar <&> toS

-- pQuotedChar :: P s Char
-- pQuotedChar = noneOf invalidQuotedChars

-- invalidQuotedChars :: [Char]
-- invalidQuotedChars =
--   "\""

-- pNakedText :: P s Text
-- pNakedText = many1 pNakedChar <&> toS

-- pNakedChar :: P s Char
-- pNakedChar = noneOf invalidNakedChars

-- invalidNakedChars :: [Char]
-- invalidNakedChars =
--   " :!\"()"

----------------------------------------

pToken :: P s Text
pToken = many1 pBlackChar <&> toS

pBlackChar :: P s Char
pBlackChar = onlyIf anyChar (not . isSpace)

-- pWhiteChar :: P s Char
-- pWhiteChar = noneOf whitespaceChars

-- whitespaceChars :: [Char]
-- whitespaceChars =
--   " \n\t"

----------------------------------------

pChromatic :: P s Chromatic
pChromatic = p <&> Chromatic
  where
  p = many1 pChroma

pChroma :: P s Chroma
pChroma = printer displayChroma 
  
pHue :: P s Hue  
pHue = printer displayHue

pColor :: P s Color  
pColor = printer displayColor

pColorIdentity :: P s ColorIdentity
pColorIdentity = printer displayColorIdentity 
  
pColorIndication :: P s ColorIndication
pColorIndication = printer displayColorIndication
  
----------------------------------------

pMana :: (Enumerable j, Show j) => P s (ManaCost j)   -- Num j
pMana = pManaSymbols <&> (Just > ManaCost) --TODO

pManaSymbols :: (Enumerable i, Show i) => P s (ManaSymbols i)
pManaSymbols = many1 p <&> ManaSymbols
 where
 p = printer displayManaSymbol 

----------------------------------------

pDate :: P s Date
pDate = pShortDate // pLongDate
  where
  pLongDate    = pYear
  
  pShortDate   = pDoubleDigit <&> disambiguateCentury
  pDoubleDigit = toDoubleDigit <$> pDigit <*> pDigit

  pYear        = fromYear <$> pDigits

  toDoubleDigit :: Int -> Int -> Int 
  toDoubleDigit x y = 10*x + 1*y
  
  disambiguateCentury :: Int -> Date
  disambiguateCentury y = 
    if y >= 93
       -- MTG was published in 1993
    then fromYear $ 1900 + y
    else fromYear $ 2000 + y

  fromYear :: Int -> YearMonthDay -- Date
  fromYear y = YearMonthDay y 0 0

    --- | y >= 93   = 1900 + y
    --- | otherwise = 2000 + y

pDigits :: (Num a) => P s a
pDigits = many1 F.number <&> toDigits
  where
  toDigits = readMay > maybe 0 fromInteger
  --  (error "[pDigits] inconceivable")

pDigit :: (Num a) => P s a
pDigit = F.number <&> toDigit
  where
  toDigit = (:[]) > readMay > maybe (error "[pDigit] inconceivable") fromInteger

----------------------------------------

pNumeric :: (Num i) => P s (Numeric i)
pNumeric = pVariable // pConstant
  where
  pVariable = Variable <$> printer displayNumericVariable 
  pConstant = Constant <$> pNumericConstant
  -- pConstant =  printer displayNumericConstant

pNumericConstant :: (Num i) => P s (NumericConstant i)
pNumericConstant = pWildcard // pLiteral
  where
  pWildcard = WildcardConstant <$  text "*"
  pLiteral  = NumericLiteral   <$> integer

----------------------------------------

unaryPrefixOperators :: [Text]
unaryPrefixOperators =
  [ "!"          
  , "-"          
  , "not"        
  ]

binaryPrefixOperators :: [Text]
binaryPrefixOperators =
  [ "or"         
  , "and"        
  ]

----------------------------------------

-- allPrefixKeywords :: SyntaxTable ()
-- allPrefixKeywords = 
--   [ "o"          -: ()
--   , "t"          -: ()
--   , "cmc"        -: ()
--   , "mana"       -: ()
--   , "c"          -: ()
--   , "ci"         -: ()
--   , "in"         -: ()
--   , "r"          -: ()
--   , "pow"        -: ()
--   , "tou"        -: ()
--   , "e"          -: ()
--   , "f"          -: ()
--   , "year"       -: ()
--   , "banned"     -: ()
--   , "legal"      -: ()
--   , "restricted" -: ()
--   , "a"          -: ()
--   , "l"          -: ()
--   , "is"         -: ()
--   , "not"        -: ()
--   , "has"        -: ()
--   ]

-- allInfixOperators :: SyntaxTable ()
-- allInfixOperators =
--   [ ":"          -: ()
--   , "!"          -: ()
--   , "="          -: ()
--   , "<"          -: ()
--   , ">"          -: ()
--   , "<="         -: ()
--   , ">="         -: ()
--   , "or"         -: ()
--   , "and"        -: ()
--   ]

-- -- | for @"is"@ and @"not"@.
-- identityKeywords :: [Text]
-- identityKeywords =
--  [ "vanilla" 
--  , "permanent"
--  , "spell"

--  , "split"
--  , "flip"

--  , "new"
--  , "old"
 
--  , "future"
--  , "timeshifted"

--  , "funny"
--  , "promo"
 
--  , "black-bordered"
--  , "white-bordered"
--  , "silver-bordered"
--  ]

-- -- | for @"has"@.
-- possessionKeywords :: [Text]
-- possessionKeywords =
--   [ "foil"
--   ]

-- ----------------------------------------

-- numericKeywords :: [Text]
-- --tsNumeric
-- numericKeywords = 
--   [ "cmc"
--   , "pow"
--   , "tou"
--   ]

-- colorKeywords :: [Text]
-- colorKeywords = 
--   [ "c"
--   , "ci"
--   , "in"
--   ]

-- colorValues :: [Text]
-- colorValues =
--   [
--   ]

-- pNumericKeywords :: P s Text
-- pNumericKeywords = texts numericKeywords

-- pColorKeywords :: P s Text
-- pColorKeywords = texts colorKeywords

---------------------------------------

-- equalityOperators_ :: [Text]
-- equalityOperators_ = [":"]

-- genericOperators :: SyntaxTable (GenericComparator)
-- genericOperators =
--   [ ":"          -: Has
--   , "!"          -: Is
--   ]

-- genericOperators_ :: [Text]
-- genericOperators_ = genericOperators & fmap fst

-- numericOperators :: SyntaxTable (NumericComparator)
-- numericOperators =
--   [ ":"          -: Equals -- HAS
--   , "!"          -: Equals -- IS
--   , "="          -: Equals
--   , "<"          -: Lesser
--   , ">"          -: Greater
--   , "<="         -: LesserEquals
--   , ">="         -: GreaterEquals
--   ]

-- numericOperators_ :: [Text]
-- numericOperators_ = numericOperators & fmap fst

-- booleanPrefixes :: SyntaxTable (GenericComparator)
-- booleanPrefixes =
--   [ "is"          -: Is
--   , "not"         -: Isnt
--   ]

-- numericOperators :: SyntaxTable (Comparison Numeric)
-- numericOperators =
--   [ ":"          -: (Contains)
--   , "!"          -: (Equals)
--   , "="          -: (Equals)
--   , "<"          -: (Lesser)
--   , ">"          -: (Greater)
--   , "<="         -: (LEQ)
--   , ">="         -: (GEQ)
--   ]

---------------------------------------  

-- parseNumericVariable :: Text -> Maybe NumericVariable
-- parseNumericVariable = display2parse displayNumericVariable

-- parseManaCost
--   :: (Enumerable i, Ord i, Show i)
--   => Text -> Maybe (ManaCost i)
-- parseManaCost = _--parseManaSymbol

parseManaSymbol
  :: (Enumerable i, Ord i, Show i)
  => Text -> Maybe (ManaSymbol i)
parseManaSymbol = display2parse displayManaSymbol

parseIs :: Text -> Maybe Is
parseIs = display2parse displayIs

----------------------------------------

display2parse
  :: (Enumerable a, Ord a)
  => (a -> Text)
  -> (Text -> Maybe a)
display2parse = injection
--display2parse f = (Map.lookup&flip) (invert' f)

----------------------------------------

{-NOTES






instance Applicative PE where
--the function (++), (,) ... but, 'text', etc, does this too
    mf <*> ma = PMap (\(f,a) -> f a) (Then mf ma)





mdo
 additive <- newRule $ do
      ((+) <$> multitive <*> char '+' *> additive)
   // multitive
 ...


mdo
 additive <- newRule $ do
      (multitive <> (char '+' *> additive) <&> uncurry (+))
   // multitive
 ...


mdo
 additive <- newRule $ do
      multitive <> char '+' ->> additive ## uncurry (+)
   // multitive
 ...



-- e.g.
additive = mdo
    additive <- newRule $ do
                  multitive <> char '+' ->> additive ## uncurry (+) // multitive
    multitive <- newRule $ do
                  primary <> char '*' ->> multitive ## uncurry (*) // primary
    primary <- newRule $ do
                  char '(' ->> additive <<- char ')' // decimal
    decimal <- newRule $ do
                  many1 (oneOf ['0' .. '9']) ## read
    return additive



onlyIf :: P s a -> (a -> Bool) -> P s a
Succeed only if thing parsed passes a predicate.


(//) :: P s a -> P s a -> P s a infixl 1 
Ordered choice, try left argument, if it fails try the right one. This does not introduce any backtracking or penalty.


runPeg :: (forall s. PM s (P s a)) -> String -> a

Run a PEG grammar. Takes the rank-2 argument in order to ensure a rule created in one PM session isn't returned and used in another PEG parser.

There is no need for special error handling, as it can be trivially implemented via

 -- parse complete file, returning 'Nothing' if parse fails
 fmap Just (myParser <<- eof) // unit Nothing
There is also no need for the parser to return its unused input, as that can be retrieved via rest.

-- Now this returns (a,String) where String is the unconsumed input.
myParser <> rest



-}
