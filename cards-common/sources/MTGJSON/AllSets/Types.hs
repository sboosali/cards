{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}

{-|

-}
module MTGJSON.AllSets.Types where

import MTGJSON.AllSets.Kinds
import MTGJSON.Extra
import MTGJSON.Types

----------------------------------------  

data Card (f :: CHARACTERISTIC -> *) = Card
  { _identifier    :: f 'IDENTIFIER

  -- gameplay-relevant stuff, card characteristic 
  , _name          :: f 'NAME
  , _manaCost      :: f 'MANACOST
  , _colors        :: f 'COLOR 
  , _type          :: f 'TYPE
  , _oracle        :: f 'ORACLE
  , _number        :: Maybe (f 'NUMBER)
  
  -- quasi-derivable stuff, card characteristics 
  , _cmc           :: f 'CMC
  , _colorIdentity :: f 'COLORIDENTITY
  , _names         :: [f 'NAME]
  , _supertypes    :: [f 'SUPERTYPE]
  , _types         :: [f 'TYPE]
  , _subtypes      :: [f 'SUBTYPE]

  -- non-gameplay-relevant stuff, card characteristics 
  , _layout        :: f 'LAYOUT
  , _watermark     :: f 'WATERMARK
  , _rarity        :: f 'RARITY
  , _flavor        :: f 'FLAVOR
  , _artist        :: f 'ARTIST
  , _ccNumber      :: f 'COLLECTORNUMBER

  -- other resources
  , _multiverseid  :: f 'MULTIVERSEID
  , _mciNumber     :: f 'COLLECTORNUMBER

  , _rulings       :: [f 'RULING]
  , _legalities    :: [f 'LEGALITY]

  , _variations    :: [f 'VARIATION]
  , _printings     :: [f 'PRINTING]
  , _originalText  :: f 'TEXT
  , _originalType  :: f 'TEXT
  , _foreignNames  :: [f 'FOREIGNVARIATION]
  } -- deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

{-| a more richly-typed 'CardObject'. i.e. parsed, simplified, and validated.

documentation for the fields is below.  

* @'_CardData_id'@: 

gameplay-relevant stuff, card characteristics: 

* @'_CardData_name'@: 
* @'_CardData_manaCost'@: 
* @'_CardData_colors'@: 
* @'_CardData_type'@: 
* @'_CardData_text'@: 
* @'_CardData_number'@: 

quasi-derivable stuff, card characteristics: 

* @'_CardData_cmc'@: 
* @'_CardData_colorIdentity'@: 
* @'_CardData_names'@: 
* @'_CardData_type'@: 

not actually derivable, without some special casing, because of color markers. e.g. Pact Of Negation has a blue dot on the left of the type line (with accompanying reminder text for people with visual impairments), which makes it blue as a characteristic. 

non-gameplay-relevant stuff, card characteristics: 

* @'_CardData_layout'@: 
* @'_CardData_watermark'@: 
* @'_CardData_rarity'@: 
* @'_CardData_flavor'@: 
* @'_CardData_artist'@: 
* @'_CardData_ccNumber'@: 

Internet data: 

* @'_CardData_multiverseid'@: 
* @'_CardData_mciNumber'@: 

links the card to other resources, like images or results from from the two standard magic card search engines (below): 

data for other printings: 
 
* @'_CardData_variations'@: 
* @'_CardData_printings'@: 
* @'_CardData_originalText'@: 
* @'_CardData_originalType'@: 
* @'_CardData_foreignNames'@: 

miscellaneous data:  

* @'_CardData_rulings'@: 
* @'_CardData_legalities'@: 

-}
data CardData = CardData 
  { _CardData_id            :: CardId 

  -- gameplay-relevant stuff, card characteristic 
  , _CardData_name          :: CardName 
  , _CardData_manaCost      :: Maybe ManaCost 
  , _CardData_colors        :: [CardColor] 
  , _CardData_type          :: CardTypeLine 
  , _CardData_oracle        :: OracleText 
  , _CardData_number        :: Maybe NumericCharacteristic
  
  -- quasi-derivable stuff, card characteristics 
  , _CardData_cmc           :: ConvertedManaCost 
  , _CardData_colorIdentity :: [CardColorIdentity] 
  , _CardData_names         :: Maybe [CardName] 
  , _CardData_supertypes    :: [CardSupertype] 
  , _CardData_types         :: (NonEmpty CardTypes) 
  , _CardData_subtypes      :: [CardSubtype] 

  -- non-gameplay-relevant stuff, card characteristics 
  , _CardData_layout        :: CardLayout 
  , _CardData_watermark     :: Maybe Text 
  , _CardData_rarity        :: Probably KnownCardRarity 
  , _CardData_flavor        :: CardFlavorText 
  , _CardData_artist        :: CardArtist
  , _CardData_ccNumber      :: CardCollectorNumber 

  -- other resources
  , _CardData_multiverseid  :: WizardsIdentifier 
  , _CardData_mciNumber     :: CardCollectorNumber -- ^ used by `MagicCards.info`, almost always identical to '_CardData_ccNumber'

  , _CardData_rulings       :: [CardRuling] 
  , _CardData_legalities    :: [CardFormatLegality]

  , _CardData_variations    :: [WizardsIdentifier] 
  , _CardData_printings     :: [CardSetCode] 
  , _CardData_originalText  :: Maybe CardText 
  , _CardData_originalType  :: Maybe CardTypeLine 
  , _CardData_foreignNames  :: [CardForeignPrinting] 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-

-- stuff I don't care about, though the Vanguard-only characteristics below can be moved up to the type-dependent-characteristics above 
, _CardData_starter       :: Bool -- IsCardStarter
, _CardData_source        :: Maybe Text
, _CardData_imageName     :: Maybe Text
, _CardData_releaseDate   :: Maybe CardReleaseDate -- ^ Promo only
, _CardData_life          :: Maybe VanguardLifeModifier -- ^ Vanguard only
, _CardData_hand          :: Maybe VanguardHandModifier -- ^ Vanguard only

-- set specific stuff: old sets, time spiral, and UN-sets 
, _CardData_reserved      :: Bool -- IsCardReserved
, _CardData_timeshifted   :: Bool -- IsCardTimeShifted
, _CardData_border        :: Maybe CardBorderColor

-}

{-| the number on the bottom-right of some cards: power and toughness, or loyalty. 

no card has either power or toughness without having both. 

no card with a power/toughness has a loyalty.

the numerical characteristic which may be
on the "south east" corner of a card.

-}
data NumericCharacteristic  
  = BodyCharacteristic    (Body    )
  | LoyaltyCharacteristic (Loyalty ) 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-|  

>>> parseBody _ "*/*+1" :: Body Printed Operator Natural
Body { bodyPower = Literal Wildcard, bodyToughness = Arithmetic Addition [Literal Wildcard, Literal 1] }

-}
data Body  = Body
  { power     :: Integer
  , toughness :: Integer
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)


data Arithmetic
  = Addition
  | Subtraction
 -- data Operator = Addition
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-|
e.g. /Tarmogoyf/:

>>> "*/1+*" :: Printed Arithmetic Natural
Literal Wildcard

(Power Toughness

-}
data Printed 
 = Literal Integer
 | Operator Arithmetic [Printed]
 | Wildcard -- ^ e.g. @*/*+1@. the @*@'s are the same. 
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

data Loyalty = Loyalty
 { getLoyalty :: Integer
 } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)
  
--   -- ^ the printed number, the most frequent case. can be negative: e.g. Char-Rumbler, which has a power of @'CardIntegerNumber' -1@. (Un-cards can have non-integer power/toughness, which we're ignoring)

--   -- ^ the integer represents the modifier: @1@ is @\*+1@, @0@ is just @\*@. e.g. Tarmogoyf has a power of  @'CardWildcardNumber' 0@ and a toughness of @'CardWildcardNumber' 1@. 
--   deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable) 

----------------------------------------

{-| unifies the following, with invariants:  

* '_CardObject_supertypes'
* '_CardObject_types'
* '_CardObject_subtypes'

-}
data CardTypes = CardTypes 
  { _CardTypes_supertypes :: [CardSupertype] 
  , _CardTypes_types      :: NonEmpty CardType 
  , _CardTypes_subtypes   :: [CardSubtype] 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| most non-creature cards have a single card type, without supertypes or subtypes. 

-}
defaultCardTypes :: CardType -> CardTypes
defaultCardTypes t = CardTypes{..} 
  where 
  _CardTypes_supertypes = []
  _CardTypes_types      = t :| [] 
  _CardTypes_subtypes   = [] 

-- TODO pair the subtype to the card type , except with the tribal supertype , which provides creature subtypes to non-creature cartoons 
{-| 

-}
data CardSupertype
  = KnownCardSupertype KnownCardSupertype 
  | UnknownCardSupertype Text 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| 

-}
data CardType
 = KnownCardType KnownCardType 
 | UnknownCardType Text 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| 

-}
data CardSubtype 
  = KnownCardSubtype KnownCardSubtype 
  | UnknownCardSubtype Text 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

{-| 

-}
newtype ConvertedManaCost = ConvertedManaCost Natural 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| the unique card identifier. 

for each set in @AllSets.json@, every card has an identifier that distinct from every other card, including the same cardname in other sets.

-}
data CardId = CardId Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data CardName = CardName Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data ManaCost = ManaCost Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-|  

-}
data CardColor = CardColor Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data CardTypeLine = CardTypeLine Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data OracleText = OracleText Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable) 

{-| 

-}
data CardText = CardText Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable) 

{-| 

-}
data CardColorIdentity = CardColorIdentity Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data CardWholeName = CardWholeName Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data CardLayout = CardLayout Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data CardWatermark = CardWatermark Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

----------------------------------------

{-| 

-}
data KnownCardRarity  
  = Common 
  | Uncommon 
  | Rare 
  | Mythic 
  | Timeshifted -- TODO
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData,Hashable)
  
{-| 

-}
newtype CardFlavorText = CardFlavorText Text 
 deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable) 

{-| 

-}
newtype CardArtist = CardArtist Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| 

-}
data CardCollectorNumber = CardCollectorNumber Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

{-| the Multiverse ID, used by `gather.wizards.com`. 

-}
data WizardsIdentifier = WizardsIdentifier Text 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| used by `MagicCards.info`, almost always identical to '_CardData_number' 

-}
data MagicCardsInfoIdentifier = MagicCardsInfoIdentifier Text 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| 

-}
data CardSetCode = CardSetCode Text 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

{-| 

-}
data CardForeignPrinting = CardForeignPrinting 
  { _CardForeignPrinting_language     :: KnownLanguage 
  , _CardForeignPrinting_name         :: CardName 
  , _CardForeignPrinting_multiverseid :: WizardsIdentifier 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| 

-}
data CardRuling = CardRuling 
  { _CardRuling_date :: Text -- TODO Day needs a hashable instance 
  , _CardRuling_text :: Text 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| 

-}
data CardFormatLegality = CardFormatLegality 
  { _CardFormatLegality_format   :: KnownMagicFormat 
  , _CardFormatLegality_legality :: KnownMagicLegality 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| 

-}
data KnownMagicFormat = KnownMagicFormat Text -- TODO 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

----------------------------------------

{-| 

-}
data KnownMagicLegality 
  = Legal 
  | Restricted 
  | Banned 
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData,Hashable)

{-| 

-}
data KnownLanguage
    = LanguageEN
    | LanguageES 
    deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData,Hashable)

data KnownCardSupertype 
    = TribalSupertype 
    | SnowSupertype 
    deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData,Hashable)

data KnownCardType 
    = InstantType 
    | SorceryType 
    deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Data,NFData,Hashable)

type KnownCardSubtype = Text -- TODO 

----------------------------------------
