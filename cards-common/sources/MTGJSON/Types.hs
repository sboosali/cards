{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.Types where

import MTGJSON.Extra
import MTGJSON.Kinds

--import Control.Lens hiding ((<&>))

--import Prelude.Spiros

----------------------------------------

{-| loosens a type. 

for example, if the "known type" @a@ is an Enum, this
"adds" infinitely many "unknown constructors" as strings.

e.g. 

-}
data Recognizable a 
  = Unrecognized String
  | Recognized   a
  deriving (Functor,Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

  -- = Unknown String
  --  Known   a

----------------------------------------

data Card (f :: CHARACTERISTIC -> *) = Card 
  { _cUID           :: f UNIQUE
  , _cMID           :: Maybe (f MULTIVERSEID)
    --TODO , _ids           :: {-Unique-}Identifiers
 
  -- gameplay-relevant stuff, card characteristic 
  , _cName          :: f NAME 
  , _cFace          :: f FACE 

  --TODO , _names         :: [f NAME] -- ^ Only used for split, flip, double-faced, and meld cards. Will contain all the names on this card, front or back. For meld cards, the first name is the card with the meld ability, which has the top half on its back, the second name is the card with the reminder text, and the third name is the melded back face.

  , _cManaCost      :: f COST   -- Cost f
  , _cCMC           :: ConvertedManaCost 

  , _cColors        :: f COLORS   -- Colors f
  -- ^ colors
  -- e.g. [ "Blue", "Green" ]
  -- The card colors. Usually this is derived from the casting cost, but some cards are special (like the back of double-faced cards and Ghostfire).
  -- @Reality Smasher@'s colors are 'colorless'.

  , _cColorIdentity :: f COLORS   -- Colors f
  -- ^ colorIdentity
  -- e.g. [ "U", "G" ]
  -- This is created reading all card color information and costs. It is the same for double-sided cards (if they have different colors, the identity will have both colors). It also identifies all mana symbols in the card (cost and text). Mostly used on commander decks.
  -- @Reality Smasher@'s colorIdentity is 'colorless' (not @[ "C" ]@, which isn't valid).

  , _cTypeline      :: Typeline f

  , _cNumeric       :: Maybe (f NUMERIC)

  -- quasi-derivable stuff, card characteristics 

  -- non-gameplay-relevant stuff, card characteristics 
  , _cRarity        :: f RARITY 
  , _cWatermark     :: Maybe (f WATERMARK)

  , _cOracle        :: f ORACLE
  , _cFlavor        :: Text 
  , _cArtist        :: Text
    --TODO CardArtist, "x & y" as two artists

  -- metagame stuff
  , _cEdition       :: f EDITION
  , _cPrintings     :: [f EDITION]      -- ^ reprints across sets

  , _cLegalities    :: [FormatLegality f]

  , _cVariations    :: [f MULTIVERSEID] -- ^ different images of the same card within a set
  , _cForeignVariations :: [ForeignVariation f]

  , _cRulings       :: [Ruling] 
  , _cOriginalText  :: Text
  , _cOriginalType  :: Text

  -- resources
  , _cAssets        :: f ASSETS --IMAGE
    --TODO , _resources     :: Resource 

  } -- deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

{-|

-}
data Typeline (f :: CHARACTERISTIC -> *) = Typeline
  { _supertypes :: List     (f 'SUPERTYPE)
  , _types      :: NonEmpty (f 'TYPE) 
  , _subtypes   :: List     (f 'SUBTYPE)
  } -- deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| most non-creature cards have a single card type, without supertypes or subtypes. 

-}
simpleCardTypes :: f TYPE -> Typeline f
simpleCardTypes x = Typeline{..} 
  where 
  _supertypes = []
  _types      = x :| [] 
  _subtypes   = []


{-

----------------------------------------
  
{-|

-}
data Cost f = Cost
  { {-_Cost-} _symbols :: List (f COST)
  } -- deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

sansManaCost :: Cost f  
sansManaCost = Cost _Cost_symbols
 where
 _symbols = []

----------------------------------------

{-|
-}
data Colors f = Colors
  (List (f COLOR))
  -- deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

colorless :: Colors f
colorless = Colors []

-- {-|
-- -}
-- data ColorIdentity f = ColorIdentity
--   { _Color_identity :: List (f COLOR)
--   } -- deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

-- colorlessIdentity :: ColorIdentity f
-- colorlessIdentity = ColorIdentity []

----------------------------------------

-}






{-

{-|

-}  
data ColorIndication f = ColorIndication
  (List (f COLOR))
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

colorlessIndication :: ColorIndication f
colorlessIndication = ColorIndication []


-}

----------------------------------------

data FormatLegality f = FormatLegality
 { _format   :: f FORMAT
 , _legality :: f LEGALITY
 } -- deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

----------------------------------------

{-| 

-}
data ForeignVariation f = ForeignVariation 
  { _Foreign_language     :: f LANGUAGE 
  , _Foreign_name         :: f NAME 
  , _Foreign_multiverseid :: Maybe (f MULTIVERSEID)
  } -- deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

-- monolingual 

----------------------------------------

{-| 

-}
data Ruling = Ruling 
  { _Ruling_date :: Date
  , _Ruling_text :: Text 
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

{-
data Resource = Resource
  { _Card_ccn           :: CollectorsNumber
  , _Card_mciNumber     :: Identifier {-CollectorsNumber-}
  -- ^ used by `MagicCards.info`, almost always identical to '_Card_number'
  } deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

simpleResource :: CollectorsNumber -> Identifier -> Resource
simpleResource ccn multiverseid
  = Resource ccn multiverseid multiverseid
-}

----------------------------------------

type Date = Text -- TODO Day needs a hashable instance

----------------------------------------

type ConvertedManaCost = CMC -- TODO 

{-| Can be added or subtracted. 

-}
newtype CMC = CMC Natural
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

{-| Unicode. 

-}
newtype Name = Name Text
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

instance IsString Name where fromString = fromString > Name

getName :: Name -> Text
getName (Name t) = t

nameless :: Name
nameless = fromString ""

{-| a mandatory card 'Name' and its optional knicknames. 

i.e.

@
Knicknames (name :| knicknames)
--
name       :: Name
knicknames :: [Name]
@

In particular, legendary creatures may refer to themselves with an abbreviation of their card name: their first name without any title/modifier. e.g.:

@
> ["Phage the Untouchable", "Phage"] :: Knicknames
@

-}
newtype Knicknames = Knicknames
  { getKnicknames :: (NonEmpty Name) }
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

-- | @fromList [] == 'knicknameless'@
instance IsList Knicknames where
  type Item Knicknames = Name
  toList   = getKnicknames > toList
  fromList = \case
    []     -> knicknameless
    (x:xs) -> Knicknames (x:|xs) 
  
  --TODO -- | 'fromList' calls @fromList NonEmpty'@, and thus is partial. 
  --  fromList = fromList > Knicknames

-- | 'knickless'
instance IsString Knicknames where
  fromString = fromString > knickless

knickless :: Name -> Knicknames
knickless name = Knicknames (name :|[])

-- | @knicknameless = 'knickless' 'nameless'@
knicknameless :: Knicknames
knicknameless = knickless nameless

-- knickless :: Name -> Knicknames
-- knickless (Name name) = Knicknames (name:|[])

knicknames2text :: Knicknames -> [Text]
knicknames2text = toList > fmap getName

----------------------------------------

type UniqueIdentifier = UID

{-| Uniquely identify a card of some set against all other cards (only by convention, not construction).

-}
data UID = UID Text
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)


----------------------------------------

type MultiverseIdentifier = MID

{-| the Multiverse ID, used by `gather.wizards.com`. 

the `MagicCards.info` number is almost always identical to this,
-}
newtype MID = MID Natural
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

--instance IsString MID where fromString = fromString > MID

----------------------------------------

type CardCollectorsNumber = CCN

{-| Each card in a set has a unique (within the set) "card collector number". 

-}
data CCN = CCN Natural Char -- Text 
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

displayCCN :: CCN -> String  --Text
displayCCN (CCN n c) = show n ++ [c]

----------------------------------------

{-

data Face f = Face
 ((f FACE) (f CARD))

    • Occurs check: cannot construct the infinite kind: k0 ~ k0 -> *
    • In the second argument of ‘f’, namely ‘f CARD’
      In the type ‘(f FACE) (f CARD)’
      In the definition of data constructor ‘Face’

-}

----------------------------------------

{-
newtype Oracle f = Oracle
 (NonEmpty (OracleFrame f))

-- { getOracle :: [

data OracleFrame f
  = OracleFrame [OracleParagraph f]

newtype OracleParagraph f
  = OracleParagraph [OraclePhrase f]

data OraclePhrase f
  = OracleSentence Text
  | OracleSymbol (f SYMBOL)

--TODO Either Symbol Text


vanilla :: Oracle
vanilla = Oracle (OracleFrame [] :| [])
-}

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
