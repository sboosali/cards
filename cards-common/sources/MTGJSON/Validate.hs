{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.Validate where

import MTGJSON.Extra
import MTGJSON.Types
import MTGJSON.Known
import MTGJSON.AllSets.Object -- Schema

import MTGJSON.Parser
import MTGJSON.Printer.Finite

import Enumerate.Function (invertInjection)
--import Data.Validation

import Data.Scientific

import qualified Data.Set as Set

import Data.List.NonEmpty (nonEmpty)

--import Prelude.Spiros


----------------------------------------

type CardValidation = Validation CardErrors

type CardErrors = NonEmpty CardError

data CardError
 = OtherCardError           String
 | BadName                  String
 | MustBeNatural            Integer
 | MustBeInteger            Double
 | UnnaturalCMC             Scientific 
 | UnknownColor             String
 | UnknownEdition           String
 | UnknownRarity            String
 | UnknownWatermark         String
 | BadManaCost              String
 | BadMagicCardsInfoURI     CardIds
 | ZeroBaseTypes            --String
 | UnknownType              String String
 | BadNumeric               String String String
 | BadLoyalty               String   -- Natural 
 | BadPower                 String 
 | BadToughness             String
 | UnknownLayout            String 
 | UnknownLanguage          String
 | WrongFaceArity           String [String]
 | UnknownFormat            String
 | UnknownLegality          String
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

  <$> (Known <$> validate_uid
                 _CardObject_id)
      --- ^^^ _uid

  <*> (fmap Known <$> validate_multiverseid
                 _CardObject_multiverseid)
      --- ^^^ _multiverseid

  <*> (Known <$> validate_name
                 _CardObject_name)
      --- ^^^ _name

  <*> (Known <$> validate_face _CardObject_names
                               _CardObject_layout)
      --- ^^^ _face

  <*> (Known <$> validate_cost
                 _CardObject_manaCost)
      --- ^^^ _cost

  <*> (id    <$> validate_cmc
                 _CardObject_cmc)
      --- ^^^ _cmc

  <*> (Known <$> validate_colors
                 _CardObject_colors)
      --- ^^^ _colors

  <*> (Known <$> validate_colorIdentity
                 _CardObject_colorIdentity)
      --- ^^^ _colorIdentity

  <*> (id    <$> validate_typeline _CardObject_supertypes
                                   _CardObject_types 
                                   _CardObject_subtypes)
      --- ^^^ _typeline

  <*> (fmap Known <$> validate_numeric'
                          _CardObject_power
                          _CardObject_toughness 
                          _CardObject_loyalty)
      --- ^^^ _numeric

  <*> (Known <$> validate_rarity
                 _CardObject_rarity)
      --- ^^^ _rarity

  <*> (fmap Known <$> validate_watermark
                      _CardObject_watermark)
      --- ^^^ _watermark

  <*> (Known <$> validate_oracle
                  (Name _CardObject_name) --TODO bindValidation
                 _CardObject_text)
      --- ^^^ _oracle

  <*> (id    <$> validate_flavor
                 _CardObject_flavor)
      --- ^^^ _flavor

  <*> (id    <$> validate_artist
                 _CardObject_artist)
      --- ^^^ _artist

  <*> (Known <$> validate_edition'
                 setCodes)
      --- ^^^ _edition

  <*> (fmap Known <$> validate_printings
                      _CardObject_printings)
      --- ^^^ _printings

  <*> (id    <$> validate_legalities
                 _CardObject_legalities)
      --- ^^^ _legalities

  <*> (fmap Known <$> validate_variations
                      _CardObject_variations)
      --- ^^^ _variations

  <*> (id    <$> validate_foreignVariations
                 _CardObject_foreignNames)
      --- ^^^ _foreignVariations

  <*> (validate_rulings
       _CardObject_rulings)
      --- ^^^ _rulings

  <*> (id    <$> validate_originalText
                 _CardObject_originalText)
      --- ^^^ _originalText

  <*> (id    <$> validate_originalType
                 _CardObject_originalType)
      --- ^^^ _originalType

  <*> (Known <$> validate_assets
       setCodes
       cardIds)
      --- ^^^ _assets

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

validateNaturalNumber :: Int -> CardValidation Natural
validateNaturalNumber (toInteger -> i)
  = i2n i
  & maybe (failure e) success 
  where
  e = MustBeNatural i

    -- mconcat [schemaFieldName, " must be a natural (non-negative integer): ", show' i]

-- validateNaturalNumber :: String -> Int -> V CardErrors Natural
-- validateNaturalNumber schemaFieldName (toInteger -> i)
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

  parseColor = invertInjection displayColorChar

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
  
validateLanguage :: Text -> CardValidation Language
validateLanguage s
  = s
  & parseLanguage
  & maybe2validation (UnknownLanguage $ toS s)

validateFormat :: Text -> CardValidation Format
validateFormat s
  = s
  & parseFormat
  & maybe2validation (UnknownFormat $ toS s)

validateLegality :: Text -> CardValidation Legality
validateLegality s
  = s
  & parseLegality
  & maybe2validation (UnknownLegality $ toS s)

----------------------------------------

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

parseLanguage :: Text -> Maybe Language
parseLanguage = toS > f --TODO text-versus-string 
  where
  f = print2parse (displayLanguage > toS)

parseFormat :: Text -> Maybe Format
parseFormat = toS > f --TODO text-versus-string 
  where
  f = print2parse (displayFormat > toS)

parseLegality :: Text -> Maybe Legality
parseLegality = toS > f --TODO text-versus-string 
  where
  f = print2parse (displayLegality > toS)

----------------------------------------
-- types

validate_typeline
  :: Maybe [Text]
  -> Maybe [Text]
  -> Maybe [Text]
  -> CardValidation (Typeline Known)
validate_typeline supertypes basetypes subtypes = Typeline
 <$> (validate_supertypes supertypes <&> fmap Known)
 <*> (validate_basetypes  basetypes  <&> fmap Known)
 <*> (validate_subtypes   subtypes   <&> fmap Known)

validate_supertypes
  :: Maybe [Text]
  -> CardValidation (List KnownSupertype)
validate_supertypes
 = maybe [] id
 > traverse validate_supertype

validate_basetypes
  :: Maybe [Text]
  -> CardValidation (NonEmpty KnownBaseType)
validate_basetypes
 = maybe [] id
 > nonEmpty'
 > either failure go -- (:|[]) 
 where
 go = traverse validate_basetype
 
 nonEmpty' :: forall a. [a] -> Either CardError (NonEmpty a)
 nonEmpty' = (nonEmpty > maybe2either (ZeroBaseTypes))
 -- nonEmpty' s
 --   = s
 --   & (nonEmpty > maybe2validation (ZeroBaseTypes s))

validate_subtypes
  :: Maybe [Text]
  -> CardValidation (List UnknownSubtype)
validate_subtypes  
 = maybe [] id
 > traverse validate_subtype

validate_supertype
  :: Text
  -> CardValidation KnownSupertype
validate_supertype supertype = go supertype
 where
 go
   = (toS > parseSupertype)
   > maybe2validation (UnknownType "super" (toS supertype))

validate_basetype
  :: Text
  -> CardValidation KnownBaseType
validate_basetype basetype
 = basetype
 & go
 where
 go
   = (toS > parseBaseType)
   > maybe2validation (UnknownType "base" (toS basetype))

validate_subtype
  :: Text
  -> CardValidation UnknownSubtype
validate_subtype = toS > success

{-
validate_subtype
  :: Text
  -> CardValidation KnownSubtype
validate_subtype subtype
 = subtype
 & go
 where
 go = maybe2validation (UnknownType "sub" (toS supertype))
-}

----------------------------------------
-- non-trivial validators
  
validate_numeric'
  :: Maybe Text
  -> Maybe Text
  -> Maybe Natural
  -> CardValidation (Maybe KnownNumeric)
validate_numeric' = validate_numeric

validate_numeric
  :: ( Integral i
     )
  => Maybe Text
  -> Maybe Text
  -> Maybe Natural
  -> CardValidation (Maybe (Numeric i))
validate_numeric Nothing      Nothing          Nothing        =
  success Nothing

validate_numeric (Just power) (Just toughness) Nothing        =
  Just <$> validate_body power toughness  

validate_numeric Nothing      Nothing          (Just loyalty) =
  Just <$> validate_loyalty loyalty

validate_numeric p            t                l              =
  failure (BadNumeric p' t' l')
  where
  p' = p & maybe "" toS
  t' = t & maybe "" toS
  l' = l & maybe "" show'

validate_body
  :: ( Integral i
     )
  => Text
  -> Text
  -> CardValidation (Numeric i)
validate_body power toughness = NumericCreature
  <$> parsePowerToughness

  where
  parsePowerToughness = Body
    <$> parsePower
    <*> parseToughness
    
  parsePower  
    = parseNumericExpression (power&toS)
    & maybe2validation (BadPower (power&toS))

  parseToughness 
    = parseNumericExpression (toughness&toS)
    & maybe2validation (BadToughness (toughness&toS))

validate_loyalty 
  :: ( Integral i
     )
  => Natural
  -> CardValidation (Numeric i)
validate_loyalty n = go n
  where
  go
    = toLoyalty
    > Just 
    > maybe2validation (BadLoyalty s)

  toLoyalty
    = n2i
    > fromInteger    --TODO replace with validator (Between's fromIntegral clips, without failing)
    > NumericLoyalty

  s = show' n

-- validate_loyalty 
--   :: ( Integral i
--      )
--   => Text
--   -> CardValidation (Numeric i)
-- validate_loyalty s = go
--   where
--   go
--     = parseLoyalty
--     > maybe2validation (BadLoyalty s)

------------------------------------------

validate_cost
  :: Maybe Text
  -> CardValidation KnownCost
validate_cost = fmap toS > validateManaCost

------------------------------------------

validate_colors
  :: Maybe [Text]
  -> CardValidation Colors 
validate_colors
  = maybe [] id
  > traverse validate_colorWord -- e.g. [ "Blue", "Green" ]
  > fmap fromList -- TODO duplicates

validate_colorWord
  :: Text
  -> CardValidation Color 
validate_colorWord (toS -> s) = go s
  where
  go
    = parseColorWord -- e.g. "Blue"
    > maybe2validation (UnknownColor s)

validate_colorIdentity
  :: Maybe [Text]
  -> CardValidation Colors -- ColorIdentity 
validate_colorIdentity
  = maybe [] id
  > traverse validate_colorChar -- e.g. [ "U", "G" ]
  > fmap fromList -- TODO duplicates
                  -- TODO "warnings" i.e. Writer not Either/Validation

validate_colorChar
  :: Text
  -> CardValidation Color 
validate_colorChar (toS -> s) = go s
  where
  go
    = parseColorChar -- e.g. "U"
    > maybe2validation (UnknownColor s)
    
------------------------------------------

validate_oracle
  :: Name
  -> Maybe Text
  -> CardValidation (Oracle)  
validate_oracle name
  = fromMaybe ""
  > toS > parseOracleLoosely names
  > success
  where
  names = knickless name --TODO

  {-
  > traverse go
  where
  go
    = parseOracleLoosely 
    -- > maybe2validation (Oracle)
  TODO ValidationWarning
  -}

------------------------------------------

validate_face
  :: Maybe [Text]
  -> Maybe Text
  -> CardValidation KnownFace
validate_face (fromMaybe [] -> names') layout'
  = either2validation $ do
  
     names <- (validate_name `traverse` names')
       & validation2either 
   
     -- NOTE monadic validation requires the Either Monad over the Validation Applicative
    
     layout <- validateLayout layout'
       & maybe2either (UnknownLayout sLayout)
       & first (:|[])
    
     face   <- validateFace names layout
       & maybe2either (WrongFaceArity sLayout sNames)
       & first (:|[])
    
     return face

 where
 sLayout = layout' & fromMaybe "" & toS
 sNames  = names' <&> toS

{-NOTE

bindValidation :: Validation e a -> (a -> Validation e b) -> Validation e b

-}


{-

  where
  names'' = names'
    & fromMaybe []
    & 


fromEither $ do
 --NOTE fromEither :: Either e a -> Validation e a
 
 -- NOTE monadic validation requires the Either Monad over the Validation Applicative
  
 names <- validate_name `traverse` names'




 either2validation $ do
     -- NOTE monadic validation requires the Either Monad over the Validation Applicative
    
     layout <- validateLayout layout'
       & maybe2either (UnknownLayout (toS layout'))
    
     face   <- validateFace names layout
       & maybe2either (WrongFaceArity (toS layout') (toS names'))
    
     return face


 names  <- validateName `traverse` names
 
 layout <- validateLayout layout'
   & maybe2validation (UnknownLayout (toS layout'))

 face   <- validateFace names layout
   & maybe2validation (WrongFaceArity (toS layout') (toS names'))

 return face
-}
  
-- validate_multiverseid
--   :: Maybe Natural
--   -> CardValidation MultiverseIdentifier
-- validate_multiverseid i = _

validateLayout
  :: Maybe Text
  -> Maybe KnownLayout -- CardValidation KnownLayout
validateLayout =
  maybe (Just NormalLayout) (toS > parseLayout)
  --- ^ normal is the default
  
  -- \case
  -- Nothing -> Just NormalLayout
  -- Just t -> parseLayout t

validateFace
  :: [Name]
  -> KnownLayout 
  -> Maybe KnownFace
validateFace names = \case
  
  NormalLayout      -> validateNullaryFace NormalFace names

  SplitLayout       -> validateBinaryFace SplitCard names
                      <&> SplitFace
  DoubleFacedLayout -> validateBinaryFace DoubleFacedCard names
                      <&> DoubleFace
  FlipLayout        -> validateBinaryFace FlipCard names
                      <&> FlipFace
  AftermathLayout   -> validateBinaryFace AftermathCard names
                      <&> AftermathFace

  MeldLayout        -> validateTernaryFace MeldCard names
                      <&> MeldFace

  _                 -> Just NormalFace -- TODO V Warning
  
------------------------------------------

validateNullaryFace
  :: f card
  -> [card]
  -> Maybe (f card)
validateNullaryFace f = \case
  [] -> Just f 
  _  -> Nothing

validateBinaryFace
  :: (card -> card -> f card)
  -> [card]
  -> Maybe (f card)
validateBinaryFace f = \case
  [x,y] -> Just $ f x y
  _     -> Nothing

validateTernaryFace
  :: (card -> card -> card -> f card)
  -> [card]
  -> Maybe (f card)
validateTernaryFace f = \case
  [x,y,z] -> Just $ f x y z
  _       -> Nothing

------------------------------------------

validate_cmc
  :: Scientific
  -> CardValidation CMC
validate_cmc s = go s
  where
  go
    = floatingOrInteger
    > either (\(_::Double) -> Nothing) i2n
    > maybe2validation (UnnaturalCMC s)
    > fmap CMC

-- validate_cmc
--   = CMC
--   > success

------------------------------------------
  
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
validate_variations
  = maybe [] id
  > fmap MID
  > success

----------------------------------------

validate_legalities
  :: Maybe [CardFormatLegalityObject]
  -> CardValidation [FormatLegality Known]
validate_legalities
  = maybe [] id
  > traverse validateFormatLegality

validateFormatLegality
  :: CardFormatLegalityObject
  -> CardValidation (FormatLegality Known)
validateFormatLegality CardFormatLegalityObject{..} = FormatLegality
  <$> validateFormat  _CardFormatLegalityObject_format
      <&> Known
  <*> validateLegality _CardFormatLegalityObject_legality 
      <&> Known

----------------------------------------


----------------------------------------

validate_foreignVariations
  :: Maybe [CardForeignPrintingObject]
  -> CardValidation [ForeignVariation Known]
validate_foreignVariations
  = maybe [] id
  > traverse validateForeignVariations

validateForeignVariations
  :: CardForeignPrintingObject
  -> CardValidation (ForeignVariation Known)
validateForeignVariations CardForeignPrintingObject{..}
 = ForeignVariation
  <$> validateLanguage _CardForeignPrintingObject_language
      <&> Known
  <*> validate_name     _CardForeignPrintingObject_name         
      <&> Known
  <*> validate_multiverseid _CardForeignPrintingObject_multiverseid
      <&> (fmap Known)

----------------------------------------

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

----------------------------------------

validate_multiverseid
  :: Maybe Natural
  -> CardValidation (Maybe MID)
validate_multiverseid
  = traverse (validateMID > maybe2validation (OtherCardError "?"))

validateMID
  :: Natural
  -> Maybe MID
validateMID
  = MID
  > Just

validateReferencedMID --TODO
  :: Set MID
  -> MID
  -> Maybe MID
validateReferencedMID declaredMIDs referncedMID
  = referncedMID
  & fromPredicate ((Set.member&flip) declaredMIDs)
  -- & maybe2validation (UndeclaredMIDWasReferenced (toS referncedMID))
  
  -- if Set.member thisMID seenMIDs
  -- then Just thisMID
  -- else Nothing -- (UndeclaredMIDWaseReferenced thisMID)

----------------------------------------

validate_name
  :: Text
  -> CardValidation Name
validate_name s = go s
  where
  go
    = validateName
    > maybe2validation (BadName (toS s))

validateName
  :: Text
  -> Maybe Name
validateName
  = Name
  > Just

validateReferencedName --TODO
  :: Set Name
  -> Name
  -> Maybe Name
validateReferencedName seenNames name =
  if Set.member name seenNames
  then Just name
  else Nothing -- (UndeclaredNameWaseReferenced name)

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
