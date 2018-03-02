{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.Known.Types where

import MTGJSON.Extra
import MTGJSON.Types (Name(..))

import Enumerate.Between

import Control.Lens (Wrapped(..))--, Iso')

import qualified Data.Text.Lazy as T

----------------------------------------

type KnownFace = Face Name

{-| Multiple "mechanical cards" can be different "faces" of the same "physical card".

This is related to, but distinct from, 'Layout'. e.g. Levelers or Vehicles have a special visual layout, but they don't (by themselves) have multiple faces, and thus are 'NormalFace'd cards. 

-}
data Face card
 = NormalFace                           -- ^ the card doesn't refernce any other card(s)
 | SplitFace     (SplitCard       card)
 | FlipFace      (FlipCard        card)
 | DoubleFace    (DoubleFacedCard card)
 | AftermathFace (AftermathCard   card)
 | MeldFace      (MeldCard        card)
 deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable)

-- = NormalFace                     card

data SplitCard card = SplitCard
 { _leftCard  :: card
 , _rightCard :: card
 } deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable)
 
data FlipCard card = FlipCard
 { _uprightFace :: card
 , _flippedFace :: card
 } deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable)
 -- { _rightsideupFace :: Card
 -- , _upsidedownFace  :: Card
 -- }
 
data AftermathCard card = AftermathCard
 { _initialFace :: card
 , _rotatedFace :: card
 } deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable)

data DoubleFacedCard card = DoubleFacedCard
 { _frontFace :: card
 , _backFace  :: card
 } deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable)

{-|

e.g. @Brisela, Voice of Nightmares (&al)@

this @mtgjson@ object:

@
{
    "layout" : "meld",
    "name" : "Bruna, the Fading Light",
    "names" : [ 
        "Gisela, the Broken Blade", 
        "Bruna, the Fading Light", 
        "Brisela, Voice of Nightmares"
    ],
}
@

is represented by this @MeldCard@ datatype:

@
brisela :: MeldCard String
brisela = MeldCard
 { _melderSmallFace   = "Gisela, the Broken Blade" 
 , _reminderSmallFace = "Bruna, the Fading Light"
 , _meldedLargeFace   = "Brisela, Voice of Nightmares"
 }
@

where

* @Gisela@ has oracle text @At the beginning of your end step, if you both own and control Gisela, the Broken Blade and a creature named Bruna, the Fading Light, exile them, then meld them into Brisela, Voice of Nightmares.@
* @Bruna@ has reminder text @(Melds with Gisela, the Broken Blade.)@

-}
data MeldCard card = MeldCard
 { _melderSmallFace   :: card
 , _reminderSmallFace :: card
 , _meldedLargeFace   :: card
 } deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable)


----------------------------------------

type KnownAssets = MCIResource  
 --TODO type KnownAssets = Maybe MCIResource

-- | metadata to build URI's for @magiccards.info@ pages (including images). 
data MCIResource = MCIResource
 { _MCI_edition    :: Text
 , _MCI_identifier :: Text
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

-- data ImageAssets = ImageAssets -- ImageIdentifiers
 -- { _Image_multiverseId          :: Maybe Natural
 -- , _Image_magiccardsinfoId      :: Maybe Text
 -- , _Image_edition               :: KnownEdition
 -- , _Image_magiccardsinfoEdition :: Text
 -- }

----------------------------------------

{-| 

-}
data Oracle
 = OracleVerbatim Text
 | OracleFrames (NonEmpty (OracleFrame))
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

-- | 'fromList' calls 'list2oracle', which only builds the 'OracleFrames' case (i.e. not 'OracleVerbatim'). 
-- 'toList' calls 'oracle2list'.
-- @[]@ becomes 'vanilla'. 
instance IsList Oracle where
  type Item Oracle = OracleFrame
  toList   = oracle2list
  fromList = list2oracle

list2oracle :: [OracleFrame] -> Oracle
list2oracle = \case
    []     -> vanilla
    (x:xs) -> OracleFrames (x:|xs)

oracle2list :: Oracle -> [OracleFrame]
oracle2list = \case
  OracleFrames   xs -> toList xs
  OracleVerbatim _  -> [] --TODO lol

-- | @~ 'Oracle'@ with singleton 'OracleFrame'.
-- @""@ becomes 'vanilla'. 
-- TODO parse with canonical rendition
instance IsString Oracle where
  fromString = \case
    "" -> vanilla
    s  -> s & (fromString > (:|[]) > OracleFrames)

{-

{-| 

-}
newtype Oracle = Oracle
 (NonEmpty (OracleFrame))
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

-- | 
-- @[]@ becomes 'vanilla'. 
instance IsList Oracle where
  type Item Oracle = OracleFrame
  toList (Oracle xs) = toList xs
  fromList = \case
    []     -> vanilla
    (x:xs) -> Oracle (x:|xs)

-- | @~ 'Oracle'@ with singleton 'OracleFrame'.
-- @""@ becomes 'vanilla'. 
-- TODO parse with canonical rendition
instance IsString Oracle where
  fromString = \case
    "" -> vanilla
    s  -> s & (fromString > (:|[]) > Oracle)
-}


-- { getOracle :: [

{-| Some cards, like levelers, have multiple "frames",
each with its own power / toughness and with distinct abilities. 

-}
data OracleFrame
  = OracleFrame [OracleParagraph]
  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

-- | (boilerplate wrapping\/unwrapping)
instance IsList OracleFrame where
  type Item OracleFrame = OracleParagraph
  toList (OracleFrame xs) = xs
  fromList = OracleFrame

-- | @~ 'OraclePhrase'@ with singleton 'OracleParagraph'.
-- TODO parse with canonical rendition
instance IsString OracleFrame where
  fromString = fromString > (:[]) > OracleFrame
  
{-| 

-}
newtype OracleParagraph
  = OracleParagraph [OracleChunk]
  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

-- | (boilerplate wrapping\/unwrapping)
instance IsList OracleParagraph where
  type Item OracleParagraph = OracleChunk
  toList (OracleParagraph xs) = xs
  fromList = OracleParagraph

-- | @~ 'OraclePhrase'@ with singleton 'OracleChunk'.
-- TODO parse with canonical rendition
instance IsString OracleParagraph where
  fromString = fromString > (:[]) > OracleParagraph
  
{-|

'OracleSymbol's are commonly rendered in text as surrounded by braces, e.g. @"{T}"@ is the tap symbol ('TapSymbol'). Unlike the text-only 'OraclePhrase's, they have a symbolic representation, for rendering when a visual interface is available. 


-}
data OracleChunk 
  = OraclePhrase [Text] -- ^ NOTE it's really a @NonEmpty Text@
  | OracleSymbol OracleSymbol --TODO KnownSymbol
  | OracleNamesake -- ^ a.k.a. @~@ or @CARDNAME@
  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)
  --TODO OracleQuoted OracleChunk -- ^ e.g. @Llanowar Mentor@. recursive. 
  --TODO OracleCost OracleChunk OracleChunk -- ^ e.g. @Llanowar Elf@. recursive, or a smaller type, without the cost grammar on the lhs, and without another on the rhs.

-- | @~ 'OraclePhrase'@
instance IsString OracleChunk where
  fromString = fromString > T.words > OraclePhrase -- TODO

----------------------------------------

{-| A vanilla card is one with no oracle
(i.e. non-reminder) text.

In this representation, vanilla cards (like all cards)
have an 'OracleFrame', but no 'OracleParagraph's. 

@
= 'Oracle' ('OracleFrame' [] ':|' [])
@

-}
vanilla :: Oracle
vanilla = OracleFrames (OracleFrame [] :| [])

-- | @(== 'vanilla')@
isVanilla :: Oracle -> Bool
isVanilla = (== vanilla)

-- newtype Oracle f = Oracle
--  (NonEmpty (OracleFrame f))

-- -- { getOracle :: [

-- data OracleFrame f
--   = OracleFrame [OracleParagraph f]

-- newtype OracleParagraph f
--   = OracleParagraph [OraclePhrase f]

-- data OraclePhrase f
--   = OracleSentence Text
--   | OracleSymbol   (f SYMBOL) -- KnownSymbol

----------------------------------------

type OracleSymbol = Either UnknownSymbol KnownSymbol --TODO

type UnknownSymbol = Text

data KnownSymbol
 = ManaSymbol          KnownManaSymbol --TODO `i`
 | MiscellaneousSymbol MiscellaneousSymbol
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

data MiscellaneousSymbol
 = TapSymbol
 | UntapSymbol
 -- LoyaltyActivationSymbol 
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

----------------------------------------

type KnownNumeric = Numeric Between_Neg5Pos25

type Between_Neg5Pos25 = Between Negative 5 Positive 25
  
{-|

e.g. Tarmogoyf is:

@
'NumericExpression' 'NumericAddition'
@

-}
data Numeric i
 = NumericCreature (Body i)
 | NumericLoyalty  i
 deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data Body i = Body
 { _power     :: NumericExpression i
 , _toughness :: NumericExpression i
 } deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data NumericExpression i
 = SimpleNumeric (NumericLiteral i)
 | BinaryNumeric NumericOperation (NumericLiteral i) (NumericLiteral i)
 deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data NumericOperation
 = NumericAddition
 | NumericSubtraction
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

--type Numeric = Either NumericConstant NumericVariable
data NumericLiteral i
  = NumericConstant i
  | NumericWildcard 
 deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

{-
  | NumericPower
  | NumericToughness
-}

----------------------------------------

type KnownChroma = Chroma

type KnownColor  = Color

data Chroma
 = Color Color
 -- TODO Hue
 | Colorless
 | SnowMana -- ^ @Snow@ has a naming conflict between 'Chroma' and 'Supertype'
 | Energy
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data Color
 = White
 | Blue
 | Black
 | Red
 | Green
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

----------------------------------------

type KnownCost = ManaCost Within20

-- | @Draco@ costs @16@. 
type Within20 = Between Positive 0 Positive 20

{-| All mana symbols are parametrized by the numeric type @i@, used by generic mana costs. When it's finite, the whole type is finite, and can thus be enumerated. 

see 'sansManaCost'. 

-}
newtype ManaCost i = ManaCost
 [ManaSymbol i]
 deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable)

instance Wrapped (ManaCost i)

-- | @= 'ManaCost' []@.
-- Which means "has no mana cost" (e.g. @Pact Of Negation@). 
sansManaCost :: ManaCost i
sansManaCost = ManaCost []

----------------------------------------

type KnownManaSymbol = ManaSymbol Within20

data ManaSymbol i
 = ChromaSymbol     Chroma
 | PhyrexianSymbol  Color
 | HybridSymbol     Guild
 -- ^ "ravnica hybrid"
 | MonoHybridSymbol Color
 -- ^ "shadowmoor hybrid"
 | GenericSymbol    i
 -- ^ @{1}, {2}, ...@
 | VariableSymbol
 -- ^ @{X}@
 deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

{-


newtype Generic
  = Generic i
  deriving (Num,Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data Variable = VariableX
  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

-- data Colorless = Colorless
-- data Snow = Snow
-- data Energy = Energy

newtype MonoHybrid
 = MonoHybrid Color
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

newtype Phyrexian
 = Phyrexian Color
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

newtype Hybrid
  = Hybrid Guild
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)


-}


----------------------------------------

-- | Two colors. like an (inductive) unordered pair of 'Color' (inlined).
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

----------------------------------------

{-|

the two colors must be different.

>>> toGuild Green Green
Nothing

toGuild is symmetric, i.e. @toGuild x y = toGuild y x@

>>> toGuild Blue Green
Just Simic

>>> toGuild Green Blue
Just Simic

-}
toGuild :: Color -> Color -> Maybe Guild

toGuild White Blue  = Just Azorius  
toGuild Blue  White = Just Azorius  

toGuild Blue  Black = Just Dimir    
toGuild Black Blue  = Just Dimir    

toGuild Black Red   = Just Rakdos   
toGuild Red   Black = Just Rakdos   

toGuild Red   Green = Just Gruul    
toGuild Green Red   = Just Gruul    

toGuild Green White = Just Selesnya 
toGuild White Green = Just Selesnya 

toGuild White Black = Just Orzhov
toGuild Black White = Just Orzhov

toGuild Black Green = Just Golgari  
toGuild Green Black = Just Golgari  

toGuild Green Blue  = Just Simic    
toGuild Blue  Green = Just Simic    

toGuild Blue  Red   = Just Izzet    
toGuild Red   Blue  = Just Izzet    

toGuild Red   White = Just Boros    
toGuild White Red   = Just Boros

toGuild White White = Nothing
toGuild Blue  Blue  = Nothing
toGuild Black Black = Nothing
toGuild Red   Red   = Nothing
toGuild Green Green = Nothing

----------------------------------------

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

----------------------------------------

-- | Three colors (i.e. either a wedge or a shard), a "slice of the color pie".
-- like an unordered triplet of 'Color' (inlined).
data Slice 
 = Shard Shard
 | Wedge Wedge
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data Shard
 = Bant
 | Esper
 | Grixis
 | Jund
 | Naya
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

data Wedge
 = Mardu
 | Temur
 | Abzan
 | Jeskai
 | Sultai
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)
         
{- | Four colors (i.e. named after the @Nephilim@). like an unordered quadruple of 'Color' (inlined).

The Nephilim from Guildpact were the first four-colored cards.
Commander 2016 introduced a second cycle of four-colored cards and named them

-}

data Nephilim
  = Artifice
  | Chaos
  | Aggression
  | Altruism
  | Growth
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

----------------------------------------

type KnownLayout = Layout

data Layout
 = NormalLayout
 | SplitLayout
 | LevelerLayout
 | FlipLayout
 | DoubleFacedLayout
 | AftermathLayout
 | MeldLayout

 | PhenomenonLayout
 | PlaneLayout
 | SchemeLayout
 | TokenLayout
 | VanguardLayout
 
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

type KnownFrame = Frame

data Frame
 = OldFrame
 | NewFrame
 | TimeshiftedFrame
 | FutureFrame
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

data Border
 = BlackBordered
 | WhiteBordered
 | SilverBordered
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

----------------------------------------

type KnownWatermark = Watermark

data Watermark
 = AbzanWatermark
 | AgentsOfSNEAKWatermark
 | AtarkaWatermark
 | AzoriusWatermark
 | BlackWatermark
 | BlueWatermark
 | BorosWatermark
 | ColorlessWatermark
 | CrossbreedLabsWatermark
 | DimirWatermark
 | DromokaWatermark
 | GoblinExplosioneersWatermark
 | GolgariWatermark
 | GreenWatermark
 | GruulWatermark
 | IzzetWatermark
 | JeskaiWatermark
 | KolaghanWatermark
 | LeagueOfDastardlyDoomWatermark
 | MarduWatermark
 | MirranWatermark
 | OjutaiWatermark
 | OrderOfTheWidgetWatermark
 | OrzhovWatermark
 | PhyrexianWatermark
 | PlaneswalkerWatermark
 | RakdosWatermark
 | RedWatermark
 | SelesnyaWatermark
 | SilumgarWatermark
 | SimicWatermark
 | SultaiWatermark
 | TemurWatermark
 | WhiteWatermark
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

{-

$ jq '.[] |= {cards: .cards | map({ watermark })}' AllSets-x.json | grep watermark | sort | uniq | cut -d '"' -f4 | grep -v -e '^[[:space:]]*$'

Abzan
Agents of S.N.E.A.K.
Atarka
Azorius
Black
Blue
Boros
Colorless
Crossbreed Labs
Dimir
Dromoka
Goblin Explosioneers
Golgari
Green
Gruul
Izzet
Jeskai
Kolaghan
League of Dastardly Doom
Mardu
Mirran
Ojutai
Order of the Widget
Orzhov
Phyrexian
Planeswalker
Rakdos
Red
Selesnya
Silumgar
Simic
Sultai
Temur
White

-}

----------------------------------------

type KnownRarity = Rarity

data Rarity
 = Common
 | Uncommon
 | Rare
 | Mythic
--TODO  | Timeshifted
--TODO | BasicLand
--TODO | Special
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

----------------------------------------

type KnownFormat = Format

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

type KnownLegality = Legality

data Legality
 = Legal
 | Restricted
 | Banned
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

data LanguageInfo = LanguageInfo
 { _languageAbbreviation :: Text
 , _languageEndonym      :: Text
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)


----------------------------------------

type KnownLanguage = Language

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

----------------------------------------

type KnownEdition = Edition

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

----------------------------------------

type KnownBlock = Block

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

----------------------------------------

type KnownSupertype = Supertype

data Supertype
 = Basic
 | Legendary
 | Snow
 | Ongoing
 | World
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

{-

$ jq '.[] |= {cards: .cards | map({ supertypes })}' AllSets-x.json | tr -s ' ' | sort|uniq| grep -P '^[a-zA-Z\s"]+$' | tr -d '"' | tr -d ' '

Basic
Legendary
Ongoing
Snow
World

-}

----------------------------------------

type KnownBaseType = BaseType

data BaseType
 = Instant
 | Sorcery
 | Land
 | Artifact
 | Enchantment
 | Creature
 | Planeswalker
 | Conspiracy
 {-
 | Scheme
 | Phenomenon
 | Plane
 | Vanguard
 -}
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

{-


$ jq '.[] |= {cards: .cards | map({ types })}' AllSets-x.json | tr
-s ' ' | sort|uniq| grep -P '^[a-zA-Z\s"]+$' | tr -d '"' | tr -d ' '

Artifact
Conspiracy
Creature
Eaturecray
Enchantment
Instant
Land
Phenomenon
Plane
Planeswalker
Scheme
See
Sorcery
Vanguard

-}

----------------------------------------

type UnknownSubtype = Text

type KnownSubtype = Subtype

--TODO
data Subtype
 = SpellSubtype         SpellType
 | LandSubtype          LandType
 | ArtifactSubtype      ArtifactType
 | EnchantmentSubtype   EnchantmentType
 | CreatureSubtype      CreatureType
 | PlaneswalkerSubtype  PlaneswalkerType
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

----------------------------------------

data family SubtypeOf (t :: BaseType)

type SpellSubtype   = SubtypeOf 'Instant

type InstantSubtype = SpellSubtype
type SorcerySubtype = SpellSubtype

data instance SubtypeOf 'Instant   
  = Arcane

----------------------------------------

data SpellType
 = SpellType
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

data LandType
 = LandType
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

data ArtifactType
 = ArtifactType
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

data EnchantmentType
 = EnchantmentType
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

data CreatureType
 = CreatureType
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

data PlaneswalkerType
 = PlaneswalkerType
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

 
{-

 = InstantSubtype InstantType
 | SorcerySubtype SorceryType


data Subtype
 = SpellType        SpellSubtype
 | LandType         LandSubtype
 | CreatureType     CreatureSubtype
 | PlaneswalkerType PlaneswalkerSubtype 
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)
-}



{-

$ jq '.[] |= {cards: .cards | map({ subtypes })}' AllSets-x.json | tr -s ' ' | sort|uniq| grep -P '^[a-zA-Z\s"]+$' | tr -d '"' | tr -d ' '


-}

----------------------------------------


