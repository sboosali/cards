
{-# LANGUAGE OverloadedLabels, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE GADTs, DeriveAnyClass #-}

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

import Enumerate

import Prelude.Spiros hiding (P)

----------------------------------------

{-| @magiccards.info@'s sytax

-}
data Syntax = Syntax
 { mciFreeform   :: [Text] -- Maybe Text
 , mciAttributes :: Attributes -- Map (Maybe Text) [Text]
 } deriving (Show,Read,Eq,Ord,Generic)

newtype Attributes = Attributes
  { getAttributes :: [Attribute] -- [(Text, Text)]
  } deriving (Show,Read,Eq,Ord,Generic) 

data Attribute = Attribute
  { subject :: Text
  , verb    :: Text
  , object  :: Text
  } deriving (Show,Read,Eq,Ord,Generic)

-- data Attribute = Attribute
--   { identifier :: Text
--   , constraint :: Text
--   } deriving (Show,Read,Eq,Ord,Generic)

-- freeform :: Text -> Syntax
-- freeform t = Syntax mciFreeform mciAttributes
--  where
--  mciFreeform    = [t]
--  mciAttributes = []

----------------------------------------

--data Comparison a =
 
data GenericComparison a =
  GenericComparison GenericComparator a a
  deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic)

data BooleanComparison a =
  BooleanComparison BooleanComparator a a
  deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic)

data NumericComparison a =
  NumericComparison NumericComparator a a
  deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic)

data GenericComparator
  = Has
  | Is
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Enumerable)

data BooleanComparator
  = Yes
  | Not
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Enumerable)

data NumericComparator 
  = Equals
  | Lesser
  | Greater
  | LesserEquals
  | GreaterEquals
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Enumerable)

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
data Numeric i
  = Constant (NumericConstant i)
  | Variable NumericVariable  
  deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,Enumerable)

data NumericConstant i
  = NumericLiteral i
  | WildcardConstant
  deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,Enumerable)

data NumericVariable
  = PowerVariable
  | ToughnessVariable
  | CostVariable
  deriving (Show,Read,Eq,Ord,Generic,Enumerable)

----------------------------------------

data Chromatic = Chromatic [Chroma]
 deriving (Show,Read,Eq,Ord,Generic)

data Chroma
 = Hue Hue
 | Multicolored
 | LandColor
 deriving (Show,Read,Eq,Ord,Generic,Enumerable)

data Hue
 = TrueColor Color
 | Colorless
 deriving (Show,Read,Eq,Ord,Generic,Enumerable)

data Color
 = White
 | Blue
 | Black
 | Red
 | Green
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Enumerable)

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

data ColorIdentity
 = ColorIdentity Hue
 deriving (Show,Read,Eq,Ord,Generic,Enumerable)

data ColorIndication
 = ColorIndication Color
 deriving (Show,Read,Eq,Ord,Generic,Enumerable)

----------------------------------------

{-| All mana symbols are parametrized by the numeric type @i@, used by generic mana costs. When it's finite, the whole type is finite, and can thus be enumerated. 

-}
data ManaCost i
 = ManaCost (Maybe (ManaSymbols i))
 deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic)

newtype ManaSymbols i = ManaSymbols 
 { getManaSymbols :: [ManaSymbol i]
 } deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic) --,Enumerable)
  
-- data ManaCost f i
--  = ManaSymbols (f (ManaSymbol i))
--  deriving (Show,Read,Eq,Ord,Generic,Enumerable)

data ManaSymbol i
 = GenericSymbol   i
 | HueSymbol       Hue
 | HybridSymbol    (Hybrid i)
 | PhyrexianSymbol Phyrexian
 deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,Enumerable)

data Hybrid i
 = GuildHybrid Guild
 | GrayHybrid i Color
 deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,Enumerable)
 --TODO GrayHybrid Natural Color
 --deriving (Show,Read,Eq,Ord,Generic)

--(UnorderedPair Color Color)

-- | like an (inductive) unordered pair of 'Color'
data Guild
 = Azorius
 | Dimir
 | Rakdos
 | Gruul
 | Selesnya
 | Orzhov
 | Izzet
 | Golgari
 | Boros
 | Simic
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Enumerable)

{-
{W}{U} Azorius Senate
{U}{B} House Dimir
{B}{R} Cult of Rakdos
{R}{G} Gruul Clans
{G}{W} Selesnya Conclave
{W}{B} Orzhov Syndicate
{U}{R} Izzet League
{B}{G} Golgari Swarm
{R}{W} Boros Legion
{G}{U} Simic Combine
-}


-- -- | inductive unordered pair
-- data Guild = UnorderedPair Color
--  deriving (Show,Read,Eq,Ord,Generic,Enumerable)
--  -- inductive set?
  
data Phyrexian
 = Phyrexian Color
 deriving (Show,Read,Eq,Ord,Generic,Enumerable)

-- data ManaSymbol
--  | ColorSymbol Color
--  | ColorlessSymbol

----------------------------------------

data Is
 = IsFace        Face
 | IsFrame       Frame
 | IsBorder      Border
 | IsPredicate   KnownPredicate
 deriving (Show,Read,Eq,Ord,Generic,Enumerable)
 
data Face
 = NormalFace
 | DoubleFace
 | SplitFace
 | FlipFace
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Enumerable)
 
data Frame
 = OldFrame
 | TimeshiftedFrame
 | NewFrame
 | FutureFrame
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Enumerable)

data Border
 = BlackBordered
 | WhiteBordered
 | SilverBordered
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Enumerable)

data KnownPredicate
 = Spell
 | Permanent
 | Vanilla
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Enumerable)

 {-

 , "funny"
 , "promo"

 -}

----------------------------------------

data Rarity
 = Common
 | Uncommon
 | Rare
 | Mythic
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Enumerable)

---------------------------------------- 

--type UnorderedPair a b = (a,b) --TODO

-- data UnorderedPair a
--  = UnorderedPair 

type Pretty a = a -> Text --TODO

----------------------------------------

type SyntaxTable a = [(Text,a)]
--type SyntaxTable a = Map Text a

----------------------------------------

{-| @magiccards.info@'s behavior, features, predicates, etc.

-}

----------------------------------------

type G s a = PM s (P s a)  
--type Grammar s a = PM s (P s a)
--type Parser  s a = P s a

type Complete a = (Maybe a, String)

----------------------------------------

