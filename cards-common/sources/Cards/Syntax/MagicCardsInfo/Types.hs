
{-# LANGUAGE OverloadedLabels, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE GADTs #-}

{-|


MagicCards.Info Syntax: <https://magiccards.info/syntax.html>


Simple Queries
====================

Name
--------------------
Birds of Paradise
"Birds of Paradise"
!Anger (Match the full name)

Rules Text (Oracle)
--------------------
o:Flying
o:"First strike"
o:{T} o:"add one mana of any color"
(new) o:"whenever ~ deals combat damage"

Types (Oracle)
--------------------
t:angel
t:"legendary angel"
t:basic
t:"arcane instant"

Colors
--------------------
c:w (Any card that is white)
c:wu (Any card that is white or blue)
c:wum (Any card that is white or blue, and multicolored)
c!w (Cards that are only white)
c!wu (Cards that are only white or blue, or both)
c!wum (Cards that are only white and blue, and multicolored)
c!wubrgm (Cards that are all five colors)
c:m (Any multicolored card)
c:l (Lands)
c:c (colorless cards)

Color Identity
--------------------
ci:wu (Any card that is white or blue, but does not contain any black, red or green mana symbols)
Color Indicator:
(new) in:wu (Any card that is white or blue according to the color indicator.)

Mana Cost
--------------------
mana=3G (Spells that cost exactly 3G, or split cards that can be cast with 3G)
mana>=2WW (Spells that cost at least two white and two colorless mana)
mana<GGGGGG (Spells that can be cast with strictly less than six green mana)
mana>=2RR mana<=6RR (Spells that cost two red mana and between two and six colorless mana)
(new) mana>={2/R}
(new) mana>={W/U}
(new) mana>={UP}

Power, Toughness, Converted Mana Cost
--------------------
pow>=8
tou<pow (All combinations are possible)
cmc=7
(new) cmc>=*

Rarity
--------------------
r:mythic

Format
--------------------
f:standard (or block, extended, vintage, classic, legacy, modern, commander)
banned:extended (or legal, restricted)

Artist
--------------------
a:"rk post"

Edition
--------------------
e:al/en (Uses the abbreviations that are listed on the sitemap)
(new) e:al,be (Cards that appear in Alpha or Beta)
(new) e:al+be (Cards that appear in Alpha and Beta)
(new) e:al,be -e:al+be (Cards that appear in Alpha or Beta but not in both editions)
(new) year<=1995 (Cards printed in 1995 and earlier)

Is
--------------------
is:split, is:flip
is:vanilla (Creatures with no card text)
is:old, is:new, is:future (Old/new/future card face)
is:timeshifted
is:funny, not:funny (Unglued/Unhinged/Happy Holidays Promos)
is:promo (Promotional cards)
is:promo is:old (Promotional cards with the original card face)
(new) is:permanent, is:spell
(new) is:black-bordered, is:white-bordered, is:silver-bordered
(new) has:foil

Language
--------------------
l:de, l:it, l:jp (Uses the abbreviations that are listed on the sitemap)


Complex Queries
====================

any Simple Query can be combined with other Simple Queries via Logical Operators to make... Complex Queries! 

e.g. "a or b", "a not b", "a -b", "a (b or c)", "a or (b c)", etc.


Logical Operators
====================

_ and _
_ _ (i.e. whitespace)

_ or _

not _
- _

( _ )


========================================
Example Queries


e.g. Saboteurs

o:"whenever ~" ((o:"deals damage to a" or o:"deals combat damage to a") (o:opponent or o:player)) or o:"attacks and isn't blocked")


========================================



-}
module Cards.Syntax.MagicCardsInfo.Types where


import Text.Parsers.Frisby (P,PM)

import Prelude.Spiros hiding (P)

----------------------------------------

{-| @magiccards.info@'s sytax

-}
data Syntax = Syntax
 { mciFreeform    :: [Text] -- Maybe Text
 , mciAttributes :: Attributes -- Map (Maybe Text) [Text]
 }

type Attributes = [Attribute] -- [(Text, Text)]

data Attribute = Attribute
  { identifier :: Text
  , constraint :: Text
  }

-- freeform :: Text -> Syntax
-- freeform t = Syntax mciFreeform mciAttributes
--  where
--  mciFreeform    = [t]
--  mciAttributes = []

----------------------------------------

data SetComparison a =
  SetComparison SetComparator a a

data NumericComparison a =
  NumericComparison NumericComparator a a

data SetComparator
  = Has
  | Is

data NumericComparator 
  = Equals
  | Lesser
  | Greater
  | LesserEquals
  | GreaterEquals

-- data Comparison a
--   HAS :: a -> a -> Comparison a
--   IS  :: a -> a -> Comparison a
--   EQ  :: a -> a -> Comparison a
--   LT  :: a -> a -> Comparison a
--   GT  :: a -> a -> Comparison a
--   LQ  :: a -> a -> Comparison a
--   GQ  :: a -> a -> Comparison a

----------------------------------------

--type Numeric = Either NumericConstant NumericVariable
data Numeric
  = Constant NumericConstant
  | Variable NumericVariable  

data NumericConstant
  = IntegerConstant Integer
  | WildcardConstant

data NumericVariable
  = PowerVariable
  | ToughnessVariable
  | CostVariable

----------------------------------------

data Chromatic = Chromatic [Chroma]

data Chroma
 = Hue Hue
 | Multicolored
 | LandColor

data Hue
  = TrueColor Color
  | Colorless

data Color
 = White
 | Blue
 | Black
 | Red
 | Green

-- data Chroma
--  = TrueColor Color
--  | Colorless 
--  | Multicolored
--  | LandColor

-- data Chroma
--  = FakeColor FakeColor
--  | Multicolored
--  | LandColor

-- data FakeColor 
--  = TrueColor Color
--  | Colorless

----------------------------------------

data ManaCost
 = ManaSymbols (Set ManaSymbol)

data ManaSymbol
 = GenericSymbol Natural
 | HueSymbol Hue
 | HybridSymbol Hybrid
 | PhyrexianSymbol Phyrexian

data Hybrid
 = ColorHybrid (UnorderedPair Color Color)
 | GrayHybrid Natural Color

data Phyrexian
 = Phyrexian Color

-- data ManaSymbol
--  | ColorSymbol Color
--  | ColorlessSymbol

---------------------------------------- 

type UnorderedPair a b = (a,b) --TODO

----------------------------------------

type SyntaxTable a = Map Text a

----------------------------------------

{-| @magiccards.info@'s behavior, features, predicates, etc.

-}

----------------------------------------

type G s a = PM s (P s a)  
--type Grammar s a = PM s (P s a)
--type Parser  s a = P s a

type Complete a = (Maybe a, String)

----------------------------------------