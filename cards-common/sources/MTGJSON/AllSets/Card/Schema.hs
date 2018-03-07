{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

{-|

Refines "MTGJSON.AllSets.Object" with: @Date@s, self-documenting Bool-like enums, and by dropping redundant (non-semantic) @Maybe@s (while keeping semantically-relevant @Maybe@s).

-}
module MTGJSON.AllSets.Card.Schema where

import MTGJSON.Extra
import MTGJSON.AllSets.Orphans()

import MTGJSON.AllSets.Enums
import MTGJSON.AllSets.Oracle

import Data.Scientific (Scientific)

import Data.Thyme.Calendar

----------------------------------------

{-| Multiple "mechanical cards" can be different "faces" of the same "physical card".

This is related to, but distinct from, 'Layout'. e.g. Levelers or Vehicles have a special visual layout, but they don't (by themselves) have multiple faces, and thus are 'NormalFace'd cards. 

-}
data Face card = Face
 { _Face_layout      :: Layout
 , _Face_currentFace :: card
 , _Face_otherFaces  :: [card]
 } deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic)

instance (NFData   card) => NFData     (Face card)
instance (Hashable card) => Hashable   (Face card)

----------------------------------------
-- CardSchema

{-|

-}
data CardSchema = CardSchema 
  { _CardSchema_identity      :: UniqueID 
  , _CardSchema_name          :: CardName
  
  , _CardSchema_multiverseid  :: MultiverseID
  , _CardSchema_mciNumber     :: Maybe Text
    -- ^ used by `MagicCards.info`, almost always identical to '_CardSchema_number'.
    -- 

  , _CardSchema_layout        :: Layout
  , _CardSchema_names         :: [CardName]

  , _CardSchema_edition       :: EditionName 
  , _CardSchema_variations    :: [MultiverseID] 
  , _CardSchema_border        :: Border

  , _CardSchema_manaCost      :: Maybe ManaCost
  , _CardSchema_cmc           :: Scientific -- Ratio Natural / Either Scientific Natural
    -- ^ Un-cards can have non-Natural converted-mana-cost. 
    -- 
  , _CardSchema_colors        :: [Color] 
  , _CardSchema_colorIdentity :: [Color]
  
  --, _CardSchema_type          :: Text 
  , _CardSchema_supertypes    :: [Supertype] 
  , _CardSchema_cardtypes     :: [Cardtype]
     -- ^ Un-cards can have no type 
  , _CardSchema_subtypes      :: [Subtype]
  
  , _CardSchema_numeric       :: NumericSchema

  , _CardSchema_rarity        :: Rarity 
  , _CardSchema_oracle        :: Oracle Text
  , _CardSchema_flavor        :: Flavor 
  , _CardSchema_artist        :: Artist
  , _CardSchema_ccn           :: CollectorNumber
    -- ^ CCN
  
  , _CardSchema_originalText  :: Maybe Text 
  , _CardSchema_originalType  :: Maybe Text
   
  , _CardSchema_reserved      :: IsReserved 
  , _CardSchema_starter       :: IsStarter 
  , _CardSchema_timeshifted   :: IsTimeshifted

  , _CardSchema_printings     :: [EditionName]
    -- ^ e.g. ["ICE", "CHR"]
    -- The sets that this card was printed in, expressed as an array of set codes.
  , _CardSchema_foreignNames  :: [ForeignPrinting] 

  , _CardSchema_legalities    :: [FormatLegality]

  , _CardSchema_rulings       :: [Ruling] 
  
  -- , _CardSchema_source        :: Maybe Text
  --   -- ^ "For promo cards, this is where this card was originally obtained. For box sets that are theme decks, this is which theme deck the card is from. For clash packs, this is which deck it is from."
  } deriving (Show,Eq,Ord,Generic)

instance NFData     CardSchema
instance Hashable   CardSchema

----------------------------------------

{-| Uniquely identify a card of some set against all other cards (only by convention, not construction).

-}
newtype UniqueID = UniqueID Text
  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

----------------------------------------

{-| the Multiverse ID, used by `gather.wizards.com`. 

the `MagicCards.info` number is almost always identical to this. 

-}
newtype MultiverseID = MultiverseID Natural
  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

--instance IsString MID where fromString = fromString > MID

----------------------------------------

newtype Flavor = Flavor Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

instance Default Flavor where def = flavorless

flavorless :: Flavor
flavorless = ""

----------------------------------------

newtype Artist = Artist Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

newtype CollectorNumber = CollectorNumber Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

----------------------------------------

data IsTimeshifted
 = NotTimeshifted
 | YesTimeshifted
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic)

instance NFData     IsTimeshifted
instance Hashable   IsTimeshifted
instance Enumerable IsTimeshifted

instance Default IsTimeshifted where def = NotTimeshifted

isTimeshifted :: Bool -> IsTimeshifted
isTimeshifted = \case
  False -> NotTimeshifted
  True  -> YesTimeshifted

----------------------------------------

data IsReserved
 = NotReserved
 | YesReserved
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic)

instance NFData     IsReserved
instance Hashable   IsReserved
instance Enumerable IsReserved

instance Default IsReserved where def = NotReserved

isReserved :: Bool -> IsReserved
isReserved = \case
  False -> NotReserved
  True  -> YesReserved

----------------------------------------

data IsStarter
 = NotStarter
 | YesStarter
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic)

instance NFData     IsStarter
instance Hashable   IsStarter
instance Enumerable IsStarter

instance Default IsStarter where def = NotStarter

isStarter :: Bool -> IsStarter
isStarter = \case
  False -> NotStarter
  True  -> YesStarter

----------------------------------------

data NumericSchema
  = NumberlessSchema
  | CreatureSchema     CreatureLike
  | PlaneswalkerSchema PlaneswalkerLike
  | VanguardSchema     VanguardLike
  deriving (Show,Read,Eq,Ord,Generic)

instance NFData     NumericSchema
instance Hashable   NumericSchema

data CreatureLike = CreatureLike
  { _Creature_power         :: NumberLike
  , _Creature_toughness     :: NumberLike
  } deriving (Show,Read,Eq,Ord,Generic)

instance NFData     CreatureLike
instance Hashable   CreatureLike

data PlaneswalkerLike = PlaneswalkerLike
  { _Planeswalker_loyalty   :: Natural
  } deriving (Show,Read,Eq,Ord,Generic)

instance NFData     PlaneswalkerLike
instance Hashable   PlaneswalkerLike

data VanguardLike = VanguardLike
  { _Vanguard_hand          :: Integer  
  , _Vanguard_life          :: Integer
  } deriving (Show,Read,Eq,Ord,Generic)

instance NFData     VanguardLike
instance Hashable   VanguardLike

----------------------------------------
  
data NumberLike
 = IntegerLike    Integer
 | ArithmeticLike Text
 deriving (Show,Read,Eq,Ord,Generic)

instance NFData     NumberLike
instance Hashable   NumberLike

-- | 'ArithmeticLike'
instance IsString NumberLike where
  fromString = fromString > ArithmeticLike

{- | @fromInteger = 'IntegerLike'@

preserves 'IntegerLike'. 

-}
instance Num NumberLike where
  fromInteger = IntegerLike

  (+) = binaryNumberLike (+) (\x y -> x <> "+" <> y)

  (*) = binaryNumberLike (*) (\x y -> x <> "*" <> y)

  abs    = unaryNumberLike abs    (\x -> "|"       <> x <> "|")
    
  signum = unaryNumberLike signum (\x -> "signum(" <> x <> ")") -- (\_ -> "?")
    
  negate = unaryNumberLike negate (\x -> "-("      <> x <> ")")

  {-NOTES

    signum :: a -> a 

    Sign of a number. The functions abs and signum should satisfy the law:

    abs x * signum x == x

    For real numbers, the signum is either -1 (negative), 0 (zero) or 1 (positive).

  -}

binaryNumberLike
  :: (Integer    -> Integer    -> Integer)
  -> (Text       -> Text       -> Text)
  -> (NumberLike -> NumberLike -> NumberLike)
binaryNumberLike operate render n₁ n₂ =
  case likeInteger of
      Right i -> IntegerLike    i
      Left  _ -> ArithmeticLike a

  where
  -- likeArithmetic
  a :: Text
  a = t₁ `render` t₂

  t₁ = asArithmeticLike n₁
  t₂ = asArithmeticLike n₂
    
  likeInteger :: Either Text Integer
  likeInteger = do
    i₁ <- isIntegerLike n₁ 
    i₂ <- isIntegerLike n₂
    return $ i₁ `operate` i₂

unaryNumberLike
  :: (Integer    -> Integer)
  -> (Text       -> Text)
  -> (NumberLike -> NumberLike)
unaryNumberLike operate render n
  = integerLike
  & bimap render operate
  & toNumberLike
  where
  integerLike :: Either Text Integer
  integerLike = isIntegerLike n

toNumberLike :: Either Text Integer -> NumberLike
toNumberLike = either ArithmeticLike IntegerLike

parseNumberLike :: Text -> NumberLike
parseNumberLike t
  = t
  & readInteger 
  & maybe2either t
  & toNumberLike

isIntegerLike :: NumberLike -> Either Text Integer
isIntegerLike = \case
  IntegerLike    i -> Right i
  ArithmeticLike t -> readInteger t & maybe2either t

asArithmeticLike :: NumberLike -> Text 
asArithmeticLike = \case
  IntegerLike    i -> show' i
  ArithmeticLike t -> t

readInteger :: Text -> Maybe Integer
readInteger t = readMay (toS t)

----------------------------------------

type ForeignPrintings = [ForeignPrinting]  

{-| 

-}
data ForeignPrinting = ForeignPrinting 
  { _Foreign_language     :: Language
  , _Foreign_name         :: CardName
  , _Foreign_multiverseid :: MultiverseID 
  } deriving (Show,Read,Eq,Ord,Generic)

instance NFData     ForeignPrinting
instance Hashable   ForeignPrinting 

----------------------------------------

type FormatLegalities = [FormatLegality]  

{-| 

-}
data FormatLegality = FormatLegality 
  { _FormatLegality_format   :: Format
  , _FormatLegality_legality :: Legality
  } deriving (Show,Read,Eq,Ord,Generic)

instance NFData     FormatLegality 
instance Hashable   FormatLegality 

----------------------------------------

type Rulings = [Ruling]

{-| 

-}
data Ruling = Ruling 
  { _Ruling_date :: Day
  , _Ruling_text :: Text 
  } deriving (Show,Eq,Ord,Generic)
  -- NOTE `thyme-Day` has no Read instance

instance NFData     Ruling
instance Hashable   Ruling

----------------------------------------

{- boilerplate templates:


instance NFData     
instance Hashable   
instance Enumerable 


newtype = Text
 deriving (Show,Eq,Ord,Generic,NFData,Hashable,IsString)


-}
