{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.Validate where

import MTGJSON.Extra
import MTGJSON.Types
import MTGJSON.Known
import MTGJSON.AllSets.Schema

import MTGJSON.Parser
import MTGJSON.Printer.Finite

import Enumerate.Function (invertInjection)
--import Data.Validation

--import qualified Data.List.NonEmpty as NonEmpty

--import Prelude.Spiros


----------------------------------------

type CardValidation = AccValidation CardErrors

type CardErrors = NonEmpty CardError

data CardError
 = MustBeNatural            Integer
 | MustBeInteger            Double
 | UnknownColor             String
 | UnknownEdition           String
 | UnknownRarity            String
 | UnknownWatermark         String
 | BadManaCost              String
 | BadMagicCardsInfoURI     CardIds
 deriving (Show,Read,Eq,Ord,Generic,NFData)
 --deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

----------------------------------------

data CardIds = CardIds
 { magicCardsInfoId :: First Text
 , multiverseId     :: First Natural
 , collectorId      :: First Text
 } deriving (Show,Read,Eq,Ord,Generic,NFData)

getCardIds :: CardObject -> CardIds
getCardIds CardObject{..} = CardIds{..}
 where
 magicCardsInfoId = First _CardObject_mciNumber
 multiverseId     = First _CardObject_multiverseid
 collectorId      = First _CardObject_number

----------------------------------------

validateCardObject
  :: SetCodes
  -> CardObject
  -> CardValidation KnownCard
validateCardObject setCodes c@CardObject{..} = Card

  <$> (Known <$> validate_uid               _CardObject_id)
      -- ^ _uid

  <*> (Known <$> validate_multiverseid      _CardObject_multiverseid)
      -- ^ _multiverseid

  <*> (Known <$> validate_name              _CardObject_name)
      -- ^ _name

  <*> (Known <$> validate_face  _CardObject_layout
                                _CardObject_names)
      -- ^ _face

  <*> (Known <$> validate_cost              _CardObject_manaCost)
      -- ^ _cost

  <*> (id    <$> validate_cmc               _CardObject_cmc)
      -- ^ _cmc

  <*> (Known <$> validate_colors            _CardObject_colors)
      -- ^ _colors

  <*> (Known <$> validate_colorIdentity     _CardObject_colorIdentity)
      -- ^ _colorIdentity

  <*> (id    <$> validate_typeline _CardObject_supertypes
                                   _CardObject_types 
                                   _CardObject_subtypes)
      -- ^ _typeline

  <*> (Known <$> validate_numeric _CardObject_power
                                  _CardObject_toughness 
                                  _CardObject_loyalty)
      -- ^ _numeric

  <*> (Known <$> validate_rarity            _CardObject_rarity)
      -- ^ _rarity

  <*> (fmap Known <$> validate_watermark         _CardObject_watermark)
      -- ^ _watermark

  <*> (Known <$> validate_oracle            _CardObject_text)
      -- ^ _oracle

  <*> (id    <$> validate_flavor            _CardObject_flavor)
      -- ^ _flavor

  <*> (id    <$> validate_artist            _CardObject_artist)
      -- ^ _artist

  <*> (Known <$> validate_edition'          setCodes)
      -- ^ _edition

  <*> (fmap Known <$> validate_printings   _CardObject_printings)
      -- ^ _printings

  <*> (id    <$> validate_legalities        _CardObject_legalities)
      -- ^ _legalities

  <*> (fmap Known <$> validate_variations        _CardObject_variations)
      -- ^ _variations

  <*> (id    <$> validate_foreignVariations _CardObject_foreignVariations)
      -- ^ _foreignVariations

  <*> (validate_rulings
       _CardObject_rulings)
      -- ^ _rulings

  <*> (id    <$> validate_originalText      _CardObject_originalText)
      -- ^ _originalText

  <*> (id    <$> validate_originalType      _CardObject_originalType)
      -- ^ _originalType

  <*> (Known <$> validate_assets
       setCodes
       cardIds)
      -- ^ _assets

  where
  cardIds = c & getCardIds
  
{-

  , _uid           :: f UNIQUE
  , _multiverseid  :: f MULTIVERSEID 
  , _name          :: f NAME 
  , _face          :: f FACE 
  , _cmc           :: ConvertedManaCost 
  , _colors        :: f COLORS   -- Colors f
  , _colorIdentity :: f COLORS   -- Colors f
  , _typeline      :: Typeline f
  , _rarity        :: f RARITY 
  , _watermark     :: f WATERMARK 
  , _oracle        :: f ORACLE
  , _flavor        :: Text 
  , _artist        :: Text
  , _assets        :: f ASSETS --IMAGE
  , _edition       :: f EDITION
  , _printings     :: [f EDITION]      
  , _legalities    :: [FormatLegality f]
  , _variations    :: [f MULTIVERSEID] 
  , _foreignVariations :: [ForeignVariation f]
  , _rulings       :: [Ruling] 
  , _originalText  :: Text
  , _originalType  :: Text

-}

----------------------------------------

validateNatural :: Int -> CardValidation Natural
validateNatural (toInteger -> i)
  = i2n i
  & maybe (failure e) success 
  where
  e = MustBeNatural i

    -- mconcat [schemaFieldName, " must be a natural (non-negative integer): ", show' i]

-- validateNatural :: String -> Int -> V CardErrors Natural
-- validateNatural schemaFieldName (toInteger -> i)
--   = i2n i
--   & maybe (failure e) success 
--   where
--   e = MustBeNatural schemaFieldName i
--     -- mconcat [schemaFieldName, " must be a natural (non-negative integer): ", show' i]

validateColors :: Maybe (List String) -> CardValidation Colors
validateColors
  = maybe [] id --  -- Set.empty id
  > fmap parseColor'
  > sequenceA
  > fmap fromList
  where
  parseColor' s = s & 
    ( parseColor
    > maybe2validation (UnknownColor s)
    )

  parseColor = invertInjection displayColor

validateManaCost
  :: (Integral i)
  => Maybe String
  -> CardValidation (ManaCost i)
validateManaCost
  = maybe "" id
  > parseManaCost'

  where
  parseManaCost' s
    = s
    & parseManaCost
    & maybe2validation (BadManaCost s)
  
validateDate :: Text -> CardValidation Date
validateDate t = success t

parseEdition :: Text -> Maybe Edition
parseEdition = toS > f --TODO text-versus-string 
  where
  f = print2parse (edition2abbreviation > toS)

parseRarity :: Text -> Maybe Rarity
parseRarity = \case
  "common"     -> Just Common
  "uncommon"   -> Just Uncommon
  "rare"       -> Just Rare
  "mythic"     -> Just Mythic
 -- "basic land" -> Just BasicLand
  _            -> Nothing

parseWatermark :: Text -> Maybe Watermark
parseWatermark = toS > f --TODO text-versus-string 
  where
  f = print2parse (displayWatermark > toS)

----------------------------------------
-- non-trivial validators

validate_typeline
  :: Maybe [Text]
  -> Maybe [Text]
  -> Maybe [Text]
  -> CardValidation (Typeline Known)
validate_typeline supertypes types subtypes = _

validate_numeric
  :: Maybe Text
  -> Maybe Text
  -> Maybe Natural
  -> CardValidation (KnownNumeric)
validate_numeric power toughness loyalty = _

validate_cost
  :: Maybe Text
  -> CardValidation KnownCost
validate_cost = fmap toS > validateManaCost

validate_oracle
  :: Maybe Text
  -> CardValidation (Oracle)  
validate_oracle t = _

validate_face
  :: Maybe Text
  -> Maybe [Text]
  -> CardValidation KnownFace
validate_face layout names = _

-- validate_multiverseid
--   :: Maybe Natural
--   -> CardValidation MultiverseIdentifier
-- validate_multiverseid i = _

validate_cmc
  :: Natural
  -> CardValidation CMC
validate_cmc
  = CMC
  > success

validate_colors
  :: Maybe [Text]
  -> CardValidation Colors 
validate_colors ts = _

validate_colorIdentity
  :: Maybe [Text]
  -> CardValidation Colors -- Identity 
validate_colorIdentity ts = _


{-|

We prioritize the @magiccards.info@-specialized identifiers\/codes;
since if present, they differ from the primary\/default ones. 

-}
validate_assets
  :: SetCodes
  -> CardIds
  -> CardValidation KnownAssets
validate_assets SetCodes{..} is@CardIds{..}
  = resource'
  & maybe2validation (BadMagicCardsInfoURI is)
  
  where
  resource'
      = MCIResource edition
    <$> identifier'  

  -- ccn = collectorId

  -- NOTE the First Applicative picks the first non-Nothing.
  
  identifier' = getFirst $ mconcat
   [ magicCardsInfoId
   , multiverseId <&> show'
   ]

  edition  = edition' & maybe primaryCode id
  edition' = getFirst $ mconcat
   [ magicCardsInfoId
   , gathererCode
   ]

  -- where
  -- _MCI_identifier = 
  --   magicCardsInfoId & maybe (multiverseId & show') Just
  -- _MCI_edition    = primaryCode magicCardsInfoCode gathererCode
  -- _MCI_ccn        = collectorId

----------------------------------------

validate_edition'
  :: SetCodes 
  -> CardValidation Edition
validate_edition' SetCodes{..} = validate_edition primaryCode

validate_edition
  :: Text
  -> CardValidation Edition
validate_edition t
  = parseEdition t
  & maybe2validation (UnknownEdition (toS t))

validate_rarity
  :: Text
  -> CardValidation Rarity 
validate_rarity t
  = parseRarity t
  & maybe2validation (UnknownRarity (toS t))

validate_watermark
  :: Maybe Text
  -> CardValidation (Maybe Watermark)
validate_watermark = \case
  Nothing -> success Nothing
  Just t  -> Just <$> go t
  where
  go t
    = parseWatermark t
    & maybe2validation (UnknownWatermark (toS t))

----------------------------------------
-- lists/records

validate_printings
  :: Maybe [Text]
  -> CardValidation [KnownEdition]
validate_printings
  = maybe [] id
  > traverse validate_printing
  where
  validate_printing e = validate_edition e

validate_variations
  :: Maybe [Natural]
  -> CardValidation [MultiverseIdentifier]
validate_variations es = _

validate_legalities
  :: Maybe [CardFormatLegalityObject]
  -> CardValidation [FormatLegality Known]
validate_legalities ls = _

validate_foreignVariations
  :: Maybe [CardForeignPrintingObject]
  -> CardValidation [ForeignVariation Known]
validate_foreignVariations fs = _

validate_ruling
  :: CardRulingObject
  -> CardValidation Ruling
validate_ruling CardRulingObject{..} = Ruling
  <$> validateDate _CardRulingObject_date
  <*> pure _CardRulingObject_text

----------------------------------------
-- boilerplate traversals

validate_rulings
  :: Maybe [CardRulingObject]
  -> CardValidation [Ruling]
validate_rulings
  = maybe [] id
  > traverse validate_ruling

----------------------------------------
-- newtypes

validate_uid
  :: Text
  -> CardValidation UID
validate_uid
  = UID
  > success

validate_name
  :: Text
  -> CardValidation Name
validate_name
  = Name
  > success

validate_multiverseid
  :: Maybe Natural
  -> CardValidation MID
validate_multiverseid
  = maybe "" show' --TODO 
  > MID
  > success

----------------------------------------
-- text

validate_flavor :: Maybe Text -> CardValidation Text
validate_flavor
  = maybe "" id
  > success
  
validate_artist :: Text -> CardValidation Text
validate_artist
  = success

validate_originalText :: Maybe Text -> CardValidation Text
validate_originalText
  = maybe "" id
  > success
  
validate_originalType :: Maybe Text -> CardValidation Text
validate_originalType
  = maybe "" id
  > success
  
----------------------------------------

