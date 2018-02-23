
--{-# LANGUAGE OverloadedLabels, DuplicateRecordFields #-}

{-# LANGUAGE ConstraintKinds, GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|


========================================

MagicCards.Info Enumerations\/Abbreviations: <https://magiccards.info/sitemap.html>

@
English English
Deutsch Deutsch
Français Français
Italiano Italiano
Español Español
Português Português    
日本語 日本語
简体中文 简体中文
Русский Русский
繁體中文 繁體中文
한국어 한국어
@


========================================

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
Editions i.e. Sets\/Blocks
TODO

Expansions
Ixalan 
RIX -> "Rivals of Ixalan" "rix"
XLN -> "Ixalan" "xln"
Amonkhet
Hour of Devastation hou
Amonkhet akh
Kaladesh
Aether Revolt aer
Kaladesh kld
Shadows over Innistrad
Eldritch Moon emn
Shadows over Innistrad soi
Battle for Zendikar
Oath of the Gatewatch ogw
Battle for Zendikar bfz
Khans of Tarkir
Dragons of Tarkir dtk
Fate Reforged frf
Khans of Tarkir ktk
Theros
Journey into Nyx jou
Born of the Gods bng
Theros ths
Return to Ravnica
Dragon's Maze dgm
Gatecrash gtc
Return to Ravnica rtr
Innistrad Cycle
Avacyn Restored avr
Dark Ascension dka
Innistrad isd
Scars of Mirrodin
New Phyrexia nph
Mirrodin Besieged mbs
Scars of Mirrodin som
Zendikar Cycle
Rise of the Eldrazi roe
Worldwake wwk
Zendikar zen
Shards of Alara
Alara Reborn arb
Conflux cfx
Shards of Alara ala
Shadowmoor Cycle
Eventide eve
Shadowmoor shm
Lorwyn Cycle
Morningtide mt
Lorwyn lw
Time Spiral Cycle
Future Sight fut
Planar Chaos pc
Time Spiral ts
Time Spiral "Timeshifted" tsts
Ice Age Cycle
Coldsnap cs
Alliances ai
Ice Age ia
Ravnica Cycle
Dissension di
Guildpact gp
Ravnica: City of Guilds rav
Kamigawa Cycle
Saviors of Kamigawa sok
Betrayers of Kamigawa bok
Champions of Kamigawa chk
Mirrodin Cycle
Fifth Dawn 5dn
Darksteel ds
Mirrodin mi
Onslaught Cycle
Scourge sc
Legions le
Onslaught on
Odyssey Cycle
Judgment ju
Torment tr
Odyssey od
Invasion Cycle
Apocalypse ap
Planeshift ps
Invasion in
Masquerade Cycle
Prophecy pr
Nemesis ne
Mercadian Masques mm
Artifacts Cycle
Urza's Destiny ud
Urza's Legacy ul
Urza's Saga us
Rath Cycle
Exodus ex
Stronghold sh
Tempest tp
Mirage Cycle
Weatherlight wl
Visions vi
Mirage mr
Early Sets
Homelands hl
Fallen Empires fe
The Dark dk
Legends lg
Antiquities aq
Arabian Nights an

Core Sets
Core Set Editions
Magic Origins ori
Magic 2015 m15
Magic 2014 Core Set m14
Magic 2013 m13
Magic 2012 m12
Magic 2011 m11
Magic 2010 m10
Tenth Edition 10e
Ninth Edition 9e
Eighth Edition 8e
Seventh Edition 7e
Classic Sixth Edition 6e
Fifth Edition 5e
Fourth Edition 4e
Revised Edition (Summer Magic "Edgar") summer
Revised Edition rv
Unlimited Edition un
Limited Edition Beta be
Limited Edition Alpha al

MTGO
Magic Online
Vintage Masters vma
MTGO Masters Edition IV me4
MTGO Masters Edition III me3
MTGO Masters Edition II me2
MTGO Masters Edition med

Special Sets
Masterpiece Series
Amonkhet Invocations mpsakh
Kaladesh Inventions mpskld
Zendikar Expeditions exp
“Conspiracy” Series
Conspiracy: Take the Crown cn2
Conspiracy cns
Premium Deck Series
Premium Deck Series: Graveborn pd3
Premium Deck Series: Fire and Lightning pd2
Premium Deck Series: Slivers pds

Reprint Sets
Iconic Masters ima
Modern Masters 2017 Edition mm3
Eternal Masters ema
Modern Masters 2015 Edition mm2
Tempest Remastered tpr
Modern Event Deck md1
Modern Masters mma
Duels of the Planeswalkers dpa
Chronicles ch

“Command Zone” Series
Explorers of Ixalan e02
Commander 2017 c17
Archenemy: Nicol Bolas e01
Commander Anthology cma
Commander 2016 c16
Planechase Anthology pca
Commander 2015 c15
Commander 2014 Edition c14
Commander 2013 Edition c13
Commander's Arsenal cma
Planechase 2012 Edition pc2
Commander cmd

Archenemy arc
Planechase pch
From The Vault
From the Vault: Transform v17
From the Vault: Lore v16
From the Vault: Angels v15
From the Vault: Annihilation v14
From the Vault: Twenty v13
From the Vault: Realms v12
From the Vault: Legends fvl
From the Vault: Relics fvr
From the Vault: Exiled fve
From the Vault: Dragons fvd

Duel Decks
Duel Decks: Merfolk vs. Goblins ddt
Duel Decks: Mind vs. Might dds
Duel Decks: Nissa vs. Ob Nixilis ddr
Duel Decks: Blessed vs. Cursed ddq
Duel Decks: Zendikar vs. Eldrazi ddp
Duel Decks Anthology: Divine vs. Demonic ddadvd
Duel Decks Anthology: Elves vs. Goblins ddaevg
Duel Decks Anthology: Garruk vs. Liliana ddagvl
Duel Decks Anthology: Jace vs. Chandra ddajvc
Duel Decks: Kiora vs. Elspeth ddo
Duel Decks: Speed vs. Cunning ddn
Duel Decks: Jace vs. Vraska ddm
Duel Decks: Heroes vs. Monsters ddl
Duel Decks: Sorin vs. Tibalt ddk
Duel Decks: Izzet vs. Golgari ddj
Duel Decks: Venser vs. Koth ddi
Duel Decks: Ajani vs. Nicol Bolas ddh
Duel Decks: Knights vs. Dragons ddg
Duel Decks: Elspeth vs. Tezzeret ddf
Duel Decks: Phyrexia vs. The Coalition pvc
Duel Decks: Garruk vs. Liliana gvl
Duel Decks: Divine vs. Demonic dvd
Duel Decks: Jace vs. Chandra jvc
Duel Decks: Elves vs. Goblins evg

Theme Decks
Coldsnap Theme Decks cstd
Independent Box Sets
Ninth Edition Box Set 9eb
Eighth Edition Box Set 8eb
Deckmasters dm
Beatdown Box Set bd
Battle Royale Box Set br
Anthologies at
Multiverse Gift Box Cards mgbc
Un-Serious Sets
Unstable ust
Unhinged uh
Unglued ug

Alternate Art
Unhinged Alternate Foils uhaa

Beginner Sets
Welcome Deck 2017 w17
Welcome Deck 2016 w16
Starter 2000 st2k
Starter 1999 st
Portal Three Kingdoms p3k
Portal Second Age po2
Portal po
Introductory Two-Player Set itp
Not Legal for Tournament Play
Collector's Edition ced
International Collectors' Edition cedi

Promo Cards
Event Incentives
Ugin's Fate ugin
15th Anniversary 15ann
Grand Prix gpx
Pro Tour pro
Magic Game Day Cards mgdc
Worlds wrl
World Magic Cup Qualifiers wmcq
Dragon Con drc
Tournament Rewards
Prerelease Events ptc
Release Events rep
Magic: The Gathering Launch Parties mlp
Summer of Magic sum
WPN/Gateway grc
Champs cp
Two-Headed Giant Tournament thgt
Arena League arena
Friday Night Magic fnmp
Magic Player Rewards mprp
Super Series sus

Gifts
Happy Holidays hho
Judge Gift Program jr
Portal Demogame pot
Redemption Rewards
European Land Program euro
Guru guru
Asia Pacific Land Program apac
WotC Online Store wotc
Celebration Cards
Celebration Cards uqc
Media Inserts
Clash Pack clash
Media Inserts mbp
Membership Incentives
Legend Membership dcilm


========================================


-}
module Cards.Syntax.MagicCardsInfo.Types where

import Enumerate

import Data.Thyme.Calendar (YearMonthDay)  

import Control.Lens hiding ((<&>))

import Prelude.Spiros hiding (P)

----------------------------------------

{-| @magiccards.info@'s raw(/ lower-level) syntax.

parametrized over @i@ and @j@, numeric types for numeric mana costs and numeric characteristics, respectively. 

-}
data Query_ i j
  = ExactQuery_     Text
  | StatementQuery_ (Statements i j) 
  deriving (Show,Eq,Ord,Generic,NFData) --,Read,Hashable)

instance Bifunctor Query_ where
  bimap fNumeric fMana = \case
    ExactQuery_      t -> ExactQuery_ t
    StatementQuery_ xs -> StatementQuery_ (xs & bimap fNumeric fMana)

data Statements i j = Statements
 { _statementFreeform   :: [Text]         -- Maybe Text
 , _statementAttributes :: Attributes i j -- Map (Maybe Text) [Text]
 } deriving (Show,Eq,Ord,Generic,NFData) --,Read,Hashable)

instance Bifunctor Statements where
  bimap fNumeric fMana = \case
    Statements x ys -> Statements x (ys & bimap fNumeric fMana)
 
instance Semigroup (Statements i j) where
  (Statements a b) <> (Statements c d) =
    Statements (a <> c) (b <> d)
           --TODO (<>) = au _Statements (<>)

instance Monoid (Statements i j) where
  mempty = Statements mempty mempty
  mappend = (<>)
  
-- emptySyntax_ ::  i j
-- emptySyntax_ =  [] emptyAttributes

{-
data Syntax_ i j
  = Syntax_ (Attributes i j)
  deriving (Show,Eq,Ord,Generic,NFData) --,Read,Hashable)

{-
  = MCIFreeform   Text
  | MCIExact      Text
  | MCIAttributes (Attributes i j)
-}  mappend = (<>)


--emptySyntax_ :: Syntax_ i j
--emptySyntax_ = Syntax_ [] emptyAttributes
-}

newtype Attributes i j = Attributes
  { getAttributes :: [Attribute i j] -- [(Text, Text)]
  } deriving (Show,Eq,Ord,Generic,NFData) --,Read,Hashable)

instance Bifunctor Attributes where
  bimap fNumeric fMana = \case
    Attributes xs -> Attributes (xs & fmap (bimap fNumeric fMana))
    
_Attributes :: Iso' (Attributes i j) [Attribute i j]
_Attributes = iso getAttributes Attributes

instance Semigroup (Attributes i j) where
  (Attributes x) <> (Attributes y) = Attributes (x <> y)
  --TODO (<>) = au _Attributes (<>)

instance Monoid (Attributes i j) where
  mempty = Attributes mempty
  mappend = (<>)
  
emptyAttributes :: Attributes i j
emptyAttributes = Attributes []
  
data Attribute i j = Attribute
  { subject :: Text
  , verb    :: Text
  , object  :: KnownAttribute i j -- AttributeObject
  } deriving (Show,Eq,Ord,Generic,NFData) --,Read,Hashable)

instance Bifunctor Attribute where
  bimap fNumeric fMana = \case
    Attribute s v o -> Attribute s v (o & bimap fNumeric fMana)
    
data KnownAttribute i j
  = TextAttribute      Text
  | DateAttribute      Date         
  | ChromaticAttribute Chromatic
  | HueAttribute       Hue
  | ColorAttribute     Color
  | NumericAttribute   (Numeric i)
  | ManaAttribute      (ManaCost j)
--X  | CostAttribute      (ManaCost j)
  deriving (Functor,Show,Eq,Ord,Generic,NFData) --,Read,Hashable)
 -- NOTE not (Date i), the `i` is for mana costs

instance Bifunctor KnownAttribute where
  bimap fNumeric fMana = \case
      NumericAttribute    i -> NumericAttribute (fNumeric <$> i)
      ManaAttribute       j -> ManaAttribute    (fMana    <$> j)
      TextAttribute       text      -> TextAttribute       text
      DateAttribute       date      -> DateAttribute       date
      ChromaticAttribute  chromatic -> ChromaticAttribute  chromatic
      HueAttribute        hue       -> HueAttribute        hue
      ColorAttribute      color     -> ColorAttribute      color

      -- x -> x

      -- TextAttribute       text      -> text
      -- DateAttribute       date      -> date
      -- ChromaticAttribute  chromatic -> chromatic
      -- HueAttribute        hue       -> hue
      -- ColorAttribute      color     -> color

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

-- 
type ParseableNumeric i = (Num i, Show i, Enumerable i)

----------------------------------------

{-| @magiccards.info@'s validated(/ higher-level) syntax.

-}
data SyntaxError
 = SyntaxError Text

----------------------------------------

{-| @magiccards.info@'s validated(/ higher-level) syntax.

-}
data Query i j = Query

----------------------------------------

--data Comparison a =
 
data GenericComparison a =
  GenericComparison GenericComparator a a
  deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable)

data BooleanComparison a =
  BooleanComparison BooleanComparator a a
  deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable)

data NumericComparison a =
  NumericComparison NumericComparator a a
  deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable)

data GenericComparator
  = Has
  | Is
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

data BooleanComparator
  = Yes
  | Not
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

data NumericComparator 
  = Equals
  | Lesser
  | Greater
  | LesserEquals
  | GreaterEquals
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

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
  deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data NumericConstant i
  = NumericLiteral i
  | WildcardConstant
  deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data NumericVariable
  = PowerVariable
  | ToughnessVariable
  | CostVariable
  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

----------------------------------------

data Chromatic = Chromatic [Chroma]
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

data Chroma
 = Hue Hue
 | Multicolored
 | LandColor
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data Hue
 = TrueColor Color
 | Colorless
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data Color
 = White
 | Blue
 | Black
 | Red
 | Green
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

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
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data ColorIndication
 = ColorIndication Color
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

----------------------------------------

{-| All mana symbols are parametrized by the numeric type @i@, used by generic mana costs. When it's finite, the whole type is finite, and can thus be enumerated. 

-}
data ManaCost i
 = ManaCost (Maybe (ManaSymbols i))
 deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable)

newtype ManaSymbols i = ManaSymbols 
 { getManaSymbols :: [ManaSymbol i]
 } deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable) --,Enumerable)
  
-- data ManaCost f i
--  = ManaSymbols (f (ManaSymbol i))
--  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data ManaSymbol i
 = GenericSymbol   i
 | HueSymbol       Hue
 | HybridSymbol    (Hybrid i)
 | PhyrexianSymbol Phyrexian
 deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data Hybrid i
 = GuildHybrid Guild
 | GrayHybrid i Color
 deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)
 --TODO GrayHybrid Natural Color
 --deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

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
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

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
--  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)
--  -- inductive set?
  
data Phyrexian
 = Phyrexian Color
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

-- data ManaSymbol
--  | ColorSymbol Color
--  | ColorlessSymbol

----------------------------------------

type Date = YearMonthDay -- Day

-- data Date
--  = Date Day
--  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

----------------------------------------

data Is
 = IsFace        Face
 | IsFrame       Frame
 | IsBorder      Border
 | IsPredicate   KnownPredicate
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)
 
data Face
 = NormalFace
 | DoubleFace
 | SplitFace
 | FlipFace
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)
 
data Frame
 = OldFrame
 | TimeshiftedFrame
 | NewFrame
 | FutureFrame
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

data Border
 = BlackBordered
 | WhiteBordered
 | SilverBordered
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

data KnownPredicate
 = Spell
 | Permanent
 | Vanilla
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

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
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

----------------------------------------

data FormatLegality
 = FormatLegality Format Legality
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data Format
 = Standard
 | Block
 | Extended
 | Vintage
 | Classic
 | Legacy
 | Modern
 | Commander
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

data Legality
 = Legal
 | Restricted
 | Banned
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

----------------------------------------

data LanguageInfo = LanguageInfo
 { _languageAbbreviation :: Text
 , _languageEndonym      :: Text
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

data Language
 = English
 | German
 | French
 | Italian
 | Spanish
 | Portuguese
 | Japanese
 | Chinese
 | Russian
 | Taiwanese
 | Korean
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

----------------------------------------

data QualifiedEdition = QualifiedEdition
 { _qEdition  :: Edition
 , _qLanguage :: Maybe Language
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data EditionInfo = EditionInfo
 { _editionBlock        :: Block
 , _editionAbbreviation :: Text
 , _editionDescription  :: Text
 --, _editionLanguages    :: [Language] --NOTE a `Set` 
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)--TODO

data Edition
 = AL
 | BE
 | UN
 | RV
 | SUMMER

 | E4
 | E5
 | E6
 | E7
 | E8
 | E9
 | E10

 | M10
 | M11
 | M12
 | M13
 | M14
 | M15
 | ORI

 | AN
 | AQ
 | LG
 | DK
 | FE
 | HL

 | MR
 | VI
 | WL

 | TP
 | SH
 | EX

 | US
 | UL
 | UD

 | MM
 | NE
 | PR

 | IN
 | PS
 | AP

 | OD
 | TR
 | JU

 | ON
 | LE
 | SC

 | MI
 | DS
 | DN5

 | CHK
 | BOK
 | SOK

 | RAV
 | GP
 | DI

 | IA
 | AI
 | CS

 | TSTS
 | TS
 | PC
 | FUT

 | LW
 | MT

 | SHM
 | EVE

 | ALA
 | CFX
 | ARB

 | ZEN
 | WWK
 | ROE

 | SOM
 | MBS
 | NPH

 | ISD
 | DKA
 | AVR

 | RTR
 | GTC
 | DGM

 | THS
 | BNG
 | JOU

 | KTK
 | FRF
 | DTK

 | BFZ
 | OGW

 | SOI
 | EMN

 | KLD
 | AER

 | AKH
 | HOU

 | XLN
 | RIX

 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

data Block 
 = Antediluvian
 | OrdinalCore
 | CardinalCore
 | EarlySets
 | MirageCycle
 | RathCycle
 | ArtifactsCycle
 | MasqueradeCycle
 | InvasionCycle
 | OdysseyCycle
 | OnslaughtCycle
 | MirrodinCycle
 | KamigawaCycle
 | RavnicaCycle
 | IceAgeCycle
 | TimeSpiralCycle
 | LorwynCycle
 | ShadowmoorCycle
 | ShardsOfAlara
 | ZendikarCycle
 | ScarsOfMirrodin
 | InnistradCycle
 | ReturnToRavnica
 | Theros
 | KhansOfTarkir
 | BattleForZendikar
 | ShadowsOverInnistrad
 | Kaladesh
 | Amonkhet
 | Ixalan
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

-- data Edition
--  = ExpansionEdition  Expansion
--  | CoreEdition       Core
--  | CommanderEdition  Commander
--  | ConspiracyEdition Conspiracy
--  | ArchenemyEdition  Archenemy
--  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

-- data Expansion
-- data Core
-- data Commander
-- data Conspiracy
-- data Archenemy

-- data Expansion

--TODO my pseudo-Blocks
--Naming: cardinal numbers: a number denoting quantity (one, two, three, etc.), as opposed to an ordinal number (first, second, third, etc.).
  
{-

Expansions...

Ixalan 
RIX -> "Rivals of Ixalan" "rix"
XLN -> "Ixalan" "xln"

Amonkhet
Hour of Devastation hou
Amonkhet akh

Kaladesh
Aether Revolt aer
Kaladesh kld

Shadows over Innistrad
Eldritch Moon emn
Shadows over Innistrad soi

Battle for Zendikar
Oath of the Gatewatch ogw
Battle for Zendikar bfz

Khans of Tarkir
Dragons of Tarkir dtk
Fate Reforged frf
Khans of Tarkir ktk

Theros
Journey into Nyx jou
Born of the Gods bng
Theros ths

Return to Ravnica
Dragon's Maze dgm
Gatecrash gtc
Return to Ravnica rtr
Innistrad Cycle
Avacyn Restored avr
Dark Ascension dka
Innistrad isd

Scars of Mirrodin
New Phyrexia nph
Mirrodin Besieged mbs
Scars of Mirrodin som

Zendikar Cycle
Rise of the Eldrazi roe
Worldwake wwk
Zendikar zen

Shards of Alara
Alara Reborn arb
Conflux cfx
Shards of Alara ala

Shadowmoor Cycle
Eventide eve
Shadowmoor shm

Lorwyn Cycle
Morningtide mt
Lorwyn lw

Time Spiral Cycle
Future Sight fut
Planar Chaos pc
Time Spiral ts
Time Spiral "Timeshifted" tsts

Ice Age Cycle
Coldsnap cs
Alliances ai
Ice Age ia

Ravnica Cycle
Dissension di
Guildpact gp
Ravnica: City of Guilds rav

Kamigawa Cycle
Saviors of Kamigawa sok
Betrayers of Kamigawa bok
Champions of Kamigawa chk

Mirrodin Cycle
Fifth Dawn 5dn
Darksteel ds
Mirrodin mi

Onslaught Cycle
Scourge sc
Legions le
Onslaught on

Odyssey Cycle
Judgment ju
Torment tr
Odyssey od

Invasion Cycle
Apocalypse ap
Planeshift ps
Invasion in

Masquerade Cycle
Prophecy pr
Nemesis ne
Mercadian Masques mm

Artifacts Cycle
Urza's Destiny ud
Urza's Legacy ul
Urza's Saga us

Rath Cycle
Exodus ex
Stronghold sh
Tempest tp

Mirage Cycle
Weatherlight wl
Visions vi
Mirage mr

Early Sets
Homelands hl
Fallen Empires fe
The Dark dk
Legends lg
Antiquities aq
Arabian Nights an


Core Set Editions...

Magic Origins ori

Magic 2015 m15
Magic 2014 Core Set m14
Magic 2013 m13
Magic 2012 m12
Magic 2011 m11
Magic 2010 m10

Tenth Edition 10e
Ninth Edition 9e
Eighth Edition 8e
Seventh Edition 7e
Classic Sixth Edition 6e
Fifth Edition 5e
Fourth Edition 4e

Revised Edition Edgar summer
Revised Edition rv
Unlimited Edition un
Limited Edition Beta be
Limited Edition Alpha al


-}

----------------------------------------

--type UnorderedPair a b = (a,b) --TODO

-- data UnorderedPair a
--  = UnorderedPair

----------------------------------------

type Print a = a    -> Text --TODO
type Parse a = Text -> Maybe a --TODO

----------------------------------------

type SyntaxTable a = [(Text,a)]
--type SyntaxTable a = Map Text a

----------------------------------------

{-| @magiccards.info@'s behavior, features, predicates, etc.

-}

----------------------------------------

