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

-- {-| loosens a type. 

-- for example, if the "known type" @a@ is an Enum, this
-- "adds" infinitely many "unknown constructors" as strings.

-- e.g. 

-- -}
-- data Probably a
--   = Unknown String
--   | Known   a
--   deriving (Functor,Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

data Card (f :: CHARACTERISTIC -> *) = Card 
  { _Card_uid           :: f UNIQUE
  , _Card_multiverseid  :: f MULTIVERSEID 
    --TODO , _Card_ids           :: {-Unique-}Identifiers
 
  -- gameplay-relevant stuff, card characteristic 
  , _Card_name          :: f NAME 
  , _Card_face          :: f FACE 

  --TODO , _Card_names         :: [f NAME] -- ^ Only used for split, flip, double-faced, and meld cards. Will contain all the names on this card, front or back. For meld cards, the first name is the card with the meld ability, which has the top half on its back, the second name is the card with the reminder text, and the third name is the melded back face.

  , _Card_manaCost      :: f COST   -- Cost f
  , _Card_cmc           :: ConvertedManaCost 

  , _Card_colors        :: f COLORS   -- Colors f
  -- ^ colors
  -- e.g. [ "Blue", "Green" ]
  -- The card colors. Usually this is derived from the casting cost, but some cards are special (like the back of double-faced cards and Ghostfire).
  -- @Reality Smasher@'s colors are 'colorless'.

  , _Card_colorIdentity :: f COLORS   -- Colors f
  -- ^ colorIdentity
  -- e.g. [ "U", "G" ]
  -- This is created reading all card color information and costs. It is the same for double-sided cards (if they have different colors, the identity will have both colors). It also identifies all mana symbols in the card (cost and text). Mostly used on commander decks.
  -- @Reality Smasher@'s colorIdentity is 'colorless' (not @[ "C" ]@, which isn't valid).

  , _Card_typeline      :: Typeline f

  -- quasi-derivable stuff, card characteristics 

  -- non-gameplay-relevant stuff, card characteristics 
  , _Card_rarity        :: f RARITY 
  , _Card_watermark     :: f WATERMARK 

  , _Card_oracle        :: f ORACLE
  , _Card_flavor        :: Text 
  , _Card_artist        :: Text
    --TODO CardArtist, "x & y" as two artists

  , _Card_assets        :: f ASSETS --IMAGE
    --TODO , _Card_resources     :: Resource 

  -- metagame stuff
  , _Card_edition       :: f EDITION
  , _Card_printings     :: [f EDITION]      -- ^ reprints across sets

  , _Card_legalities    :: [FormatLegality f]

  , _Card_variations    :: [f MULTIVERSEID] -- ^ different images of the same card within a set
  , _Card_foreignVariations :: [ForeignVariation f]

  , _Card_rulings       :: [Ruling] 
  , _Card_originalText  :: Text
  , _Card_originalType  :: Text

  } -- deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

{-|

-}
data Typeline (f :: CHARACTERISTIC -> *) = Typeline
  { _Card_supertypes :: List     (f 'SUPERTYPE)
  , _Card_types      :: NonEmpty (f 'TYPE) 
  , _Card_subtypes   :: List     (f 'SUBTYPE)
  } -- deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

{-| most non-creature cards have a single card type, without supertypes or subtypes. 

-}
simpleCardTypes :: f TYPE -> Typeline f
simpleCardTypes x = Typeline{..} 
  where 
  _Card_supertypes = []
  _Card_types      = x :| [] 
  _Card_subtypes   = []


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
 { _Card_format   :: f FORMAT
 , _Card_legality :: f LEGALITY
 } -- deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,Enumerable)

----------------------------------------

{-| 

-}
data ForeignVariation f = ForeignVariation 
  { _Foreign_language     :: f LANGUAGE 
  , _Foreign_name         :: f NAME 
  , _Foreign_multiverseid :: f MULTIVERSEID
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

{-| Can be added or subtracted. 

-}
newtype ConvertedManaCost = ConvertedManaCost Natural
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

{-| Unicode. 

-}
newtype Name = Name Text
 deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

{-| Uniquely identify a card of some set against all other cards (only by convention, not construction).

-}
data UniqueIdentifier = UniqueIdentifier Text
  deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)


----------------------------------------

{-| the Multiverse ID, used by `gather.wizards.com`. 

the `MagicCards.info` number is almost always identical to this,
-}
data MultiverseIdentifier =  MultiverseIdentifier Text 
  deriving (Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

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
