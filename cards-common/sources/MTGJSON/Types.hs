{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.Types where

import MTGJSON.Extra
import MTGJSON.Kinds

import Control.Lens hiding ((<&>))

import Prelude.Spiros

----------------------------------------

{-| loosens a type. 

for example, if the "known type" @a@ is an Enum, this
"adds" infinitely many "unknown constructors" as strings.

e.g. 

-}
data Probably a
  = Unknown String
  | Known   a
  deriving (Functor,Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

data Card (f :: CHARACTERISTIC) = Card 
  { _Card_uid           :: UniqueIdentifier
  , _Card_ids           :: {-Unique-}Identifiers
 
  -- gameplay-relevant stuff, card characteristic 
  , _Card_name          :: Name 
  , _Card_manaCost      :: Maybe ManaCost 
  , _Card_colors        :: [Color] 
  , _Card_typeline      :: Typeline f
  , _Card_text          :: Oracle

  -- quasi-derivable stuff, card characteristics 
  , _Card_cmc           :: ConvertedManaCost 
  , _Card_colorIdentity :: [CardColorIdentity] 
  , _Card_names         :: Maybe [CardName] 

  -- non-gameplay-relevant stuff, card characteristics 
  , _Card_layout        :: CardLayout 
  , _Card_rarity        :: KnownCardRarity 
  , _Card_editioin      :: Known Edition
  , _Card_watermark     :: Maybe Text 
  , _Card_flavor        :: CardFlavorText 
  , _Card_artist        :: Text --TODO CardArtist, "x & y" as two artists

  -- other resources
  , _Card_resources     :: Resource 

  -- metagame stuff
  , _Card_rulings       :: [CardRuling] 
  , _Card_legalities    :: [CardFormatLegality]

  , _Card_variations    :: [WizardsIdentifier] 
  , _Card_printings     :: [CardSetCode] 
  , _Card_originalText  :: Maybe CardText 
  , _Card_originalType  :: Maybe CardTypeLine 
  , _Card_foreignNames  :: [CardForeignPrinting] 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

{-|

-}
data Typeline (f :: CHARACTERISTIC) = Typeline
  { _Card_supertypes :: List     (f 'SUPERTYPE)
  , _Card_types      :: NonEmpty (f 'TYPE) 
  , _Card_subtypes   :: List     (f 'SUBTYPE)
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| most non-creature cards have a single card type, without supertypes or subtypes. 

-}
simpleCardTypes :: f TYPE -> CardTypes f
simpleCardTypes x = CardTypes{..} 
  where 
  _CardTypes_supertypes = []
  _CardTypes_types      = x :| [] 
  _CardTypes_subtypes   = [] 

----------------------------------------

data Resource = Resource
  { _Card_ccn           :: CollectorsNumber
  , _Card_multiverseid  :: {-Wizards-}Identifier 
  , _Card_mciNumber     :: Identifier {-CollectorsNumber-}
  -- ^ used by `MagicCards.info`, almost always identical to '_Card_number'
  }

simpleResource :: CollectorsNumber -> Identifier -> Resource
simpleResource ccn multiverseid
  = Resource ccn multiverseid multiverseid

----------------------------------------

data FormatLegality f = FormatLegality
 { _Card_format   :: Format
 , _Card_legality :: Legality
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

----------------------------------------

{-| 

-}
data Printing f = Printing 
  { _Printing_language     :: KnownLanguage 
  , _Printing_name         :: Name 
  , _Printing_multiverseid :: WizardsIdentifier 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

{-| 

-}
data Ruling = Ruling 
  { _Ruling_date :: Date
  , _Ruling_text :: Text 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

type Date = Text -- TODO Day needs a hashable instance

----------------------------------------

newtype Oracle = Oracle
 (NonEmpty OracleFrame)

-- { getOracle :: [

data OracleFrame
  = OracleFrame [OracleParagraph]

newtype OracleParagraph
  = OracleParagraph [OraclePhrase]

data OraclePhrase
  = OracleSentence Text
  | OracleSymbol Symbol

--TODO Either Symbol Text


vanilla :: Oracle
vanilla = Oracle (OracleFrame [] :| [])

-- newtype Oracle = Oracle [OracleParagraph]

-- newtype OracleParagraph = OracleParagraph OraclePhrase

-- data OraclePhrase
--   = OracleSentence Text
--   | OracleSymbol Symbol
--   | OracleFrame Frame 

-- --TODO Either Symbol Text

-- -- { getOracle :: [

-- data Symbol
--  = TapSy

----------------------------------------
  
data Symbol
 = TapSymbol
 | UntapSymbol
 | ManaSymbol () --TODO
 | LoyaltySymbol ()

----------------------------------------

{-| 

-}
data UniqueIdentifier = UniqueIdentifier Text
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| Each card in a set has a unique "card collector number". 

-}
data CollectorNumber = CollectorNumber Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| the Multiverse ID, used by `gather.wizards.com`. 

the `MagicCards.info` number is almost always identical to this,
-}
data MultiverseIdentifier =  MultiverseIdentifier Text 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

data CardFaces
 = NormalFace    Card
 | SplitFace     SplitCard
 | DoubleFace    DoubleFacedCard
 | FlipFace      FlipCard
 | AftermathFace AftermathCard
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

data SplitCard = SplitCard
 { _leftCard  :: Card
 , _rightCard :: Card
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

data DoubleFacedCard = DoubleFacedCard
 { _frontFace :: Card
 , _backFace  :: Card
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)
 
data FlipCard = FlipCard
 { _uprightFace :: Card
 , _flippedFace :: Card
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)
 -- { _rightsideupFace :: Card
 -- , _upsidedownFace  :: Card
 -- }
 
data AftermathCard = AftermathCard
 { _initialFace :: Card
 , _rotatedFace :: Card
 } deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

----------------------------------------

{-|

e.g. Tarmogoyf is:

@
'NumericExpression' 'NumericAddition'
@

-}
data NumericCharacteristic i
 = NumericCreature (Body i)
 | NumericLoyalty  i

data Body i = Body
 { _power     :: NumericExpression i
 , _toughness :: NumericExpression i
 } deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data NumericExpression i
 = SimpleNumeric (Numeric i)
 | NumericExpression NumericOperation (Numeric i) (Numeric i)
 deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data NumericOperation
 = NumericAddition
 deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

--type Numeric = Either NumericConstant NumericVariable
data Numeric i
  = NumericConstant i
  | NumericWildcard 
  | NumericPower
  | NumericToughness
  deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

----------------------------------------

{-| Represents a @Set@. 

-}
data Chromatic = Chromatic [Chroma]
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

data Chroma
 = Colorful Color
 | Colorless
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data Color
 = White
 | Blue
 | Black
 | Red
 | Green
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

----------------------------------------

data ColorIdentity
 = ColorIdentity Chroma
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

data ColorIndication
 = ColorIndication Color
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

----------------------------------------

{-| All mana symbols are parametrized by the numeric type @i@, used by generic mana costs. When it's finite, the whole type is finite, and can thus be enumerated. 

@ManaCost []@ means "has no mana cost". 

-}
newtype ManaCost i
 = ManaCost [ManaSymbol i]
 deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic,NFData,Hashable)

instance Wrapped (ManaCost i)

----------------------------------------

data ManaSymbol i
 = ColorSymbol Color
 | GenericSymbol i
 | VariableSymbol --Variable
 | ColorlessSymbol
 | SnowSymbol
 | EnergySymbol 
 | MonoHybridSymbol Color
 | PhyrexianSymbol Color
 | HybridSymbol Guild
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
 | Azban
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

data Layout
 = Aftermath
 | DoubleFaced
 | Flip
 | Leveler
 | Meld
 | Normal
 | Phenomenon
 | Plane
 | Scheme
 | Split
 | Token
 | Vanguard
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

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

data Rarity
 = Basic
 | Common
 | Uncommon
 | Rare
 | Mythic
--TODO | Special
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

----------------------------------------

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

----------------------------------------

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
