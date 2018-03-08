
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

{-|

-}
module MTGJSON.AllSets.Validate where
import MTGJSON.Extra

import MTGJSON.AllSets.Enums
import MTGJSON.AllSets.Booster
import MTGJSON.AllSets.Oracle

import MTGJSON.AllSets.Object as Object
import MTGJSON.AllSets.Set    as Edition
import MTGJSON.AllSets.Card   as Card

--

import Data.Fuzzy

--

import "thyme"      Data.Thyme
import "old-locale" System.Locale (defaultTimeLocale)
--import qualified "attoparsec" Data.Attoparsec.ByteString as Attoparsec

--

import qualified Data.Map as Map

--

-- import Data.Monoid
-- import Prelude.Spiros

----------------------------------------
  
{-|

@

@

-}
validateSets
  :: [SetObject]
  -> Map Query (Edition, [CardObject], [CardSchema])
validateSets sets = Map.empty

{-|

e.g.

@
((edition, cards) : _) <- validateSetsM xs
@


@
:set -XOverloadedStrings
import Control.Lens

isRIX = \edition -> (edition ^. edition_code) == "RIX" || T.toCaseFold (edition ^. edition_name) == T.toCaseFold "Rivals of Ixalan"

ys <- validateSetsM xs
(rixEdition, rixCards) <- ys ^?! (filtered isRIX)


@

-}
validateSetsM
  :: (MonadThrow m)
  => [SetObject]
  -> m [(Edition, [CardSchema])]
validateSetsM sets = return []

-- Just (rixEdition, rixCards) <- ys ^? (traverse . filtered . edition_code )
  
----------------------------------------

{-|

@
Editions es <- validateEditionsStrictlyM os
@

-}
validateEditionsStrictlyM
  :: ( MonadThrow m
     )
  => [SetObject]
  -> m Editions
validateEditionsStrictlyM
 = validateEditionsStrictly
 > maybe2throw 

validateEditionsStrictly :: [SetObject] -> Maybe Editions
validateEditionsStrictly 
 = traverse validateEdition
 > fmap Editions

{-|

@
(invalids, valids) = validateEdition _
@


@

Tenth Edition
Unlimited Edition
Revised Edition
Fourth Edition
Fifth Edition
Classic Sixth Edition
Seventh Edition
Eighth Edition
Ninth Edition
Archenemy
Arabian Nights
Antiquities
Commander 2013 Edition
Commander 2014
Commander 2015
Commander 2016
Commander 2017
Magic: The Gathering-Commander
Conspiracy: Take the Crown
Magic: The Gathering—Conspiracy
The Dark
Eternal Masters
Fallen Empires
Homelands
Planechase
Iconic Masters
Limited Edition Alpha
Limited Edition Beta
Legends
Magic 2010
Magic 2011
Magic 2012
Magic 2013
Magic 2014 Core Set
Magic 2015 Core Set
Modern Masters 2015 Edition
Modern Masters 2017 Edition
Modern Masters
Magic Origins
Planechase 2012 Edition
Portal Second Age
Portal
Portal Three Kingdoms
Rivals Quick Start Set
Tempest Remastered
Unglued
Unhinged
Unstable
Vanguard
Vintage Masters

@

-}
validateEditions :: [SetObject] -> ([SetObject], Editions)
validateEditions xs
   = zip xs ys
   & go

 where
 ys = validateEdition <$> xs

 go 
   = fmap (maybe2either' &uncurry)
   > partitionEithers
   > second Editions
   
 maybe2either' :: e -> Maybe a -> Either e a
 maybe2either' invalid valid
   = valid & maybe2either invalid
   -- = valid & either (const invalid) 

----------------------------------------

toUniformBooster :: MagicBoosterObject -> Booster
toUniformBooster = fmap toUniformBoosterSlot > Booster

toUniformBoosterSlot :: MagicBoosterSlotObject -> BoosterSlot
toUniformBoosterSlot = fromMagicBoosterSlotObject > uniformBoosterSlot

{-
  = fmap go
  > catMaybes
  where
  go
    = fromMagicBoosterSlotObject
    > nonEmpty
    > fmap toBooster
-}

----------------------------------------

getEditionCodes :: SetObject -> EditionCodes
getEditionCodes SetObject{..} = EditionCodes{..}
 where
 _Edition_primaryCode        = _SetObject_code
 
 _Edition_gathererCode       = _SetObject_gathererCode
   & fromMaybe _Edition_primaryCode

 _Edition_oldCode            = _SetObject_oldCode
   & fromMaybe _Edition_gathererCode

 _Edition_magicCardsInfoCode = _SetObject_magicCardsInfoCode
   & id
   -- TODO don't default on absence
   -- & fromMaybe _Edition_gathererCode

----------------------------------------

{-|

'validateEdition', and if the set metadata is valid, 'validateCard' each card, returning as many valid cards as possible. 

@
(edition,successes,failures) <- 'validateSetM' s
@

-}
validateSet :: SetObject -> Maybe (Edition, [CardObject], [CardSchema])
validateSet _SetObject@SetObject{..} = do
  
  edition@Edition{..} <- validateEdition _SetObject
  
  let (successes,failures) = _SetObject_cards
        & validateCards _Edition_name _Edition_border
        
  return (edition,successes,failures)

{-|

-}
validateSetM
  :: (MonadThrow m)
  => SetObject
  -> m (Edition, [CardObject], [CardSchema])
validateSetM = validateSet > maybe2throw
 
----------------------------------------

{-|




-}
validateEdition :: SetObject -> Maybe Edition
validateEdition _SetObject@SetObject{..} = do

  releaseDate          <- _SetObject_releaseDate
  _Edition_releaseDate <- parseDay releaseDate 

  -- block                <- _SetObject_block 
  -- let _Edition_block = BlockName block
  
  pure$ Edition{..}
 
 where
 _Edition_name        = _SetObject_name         &  EditionName
 _Edition_codes       = _SetObject              &  getEditionCodes
 _Edition_type        = _SetObject_type         &  EditionType
 _Edition_border      = _SetObject_border       &  maybe blackBorder Border
 _Edition_block       = _SetObject_block       <&> BlockName 

 _Edition_booster     = _SetObject_booster
   &  maybe defaultBooster toUniformBooster

 _Edition_onlineOnly  = _SetObject_onlineOnly
   &  maybe OfflineToo fromOnlineOnly

-- _Edition_releaseDate = _SetObject_releaseDate <&> (parseDay > join)

-- validateEdition :: SetObject -> Edition
-- validateEdition _SetObject@SetObject{..} = Edition{..}
 
----------------------------------------

{-|

@
(successes,failures) <- 'validateCards' edition border cards
@

-}
validateCards
  :: EditionName -> Border 
  -> [CardObject]
  -> ([CardObject], [CardSchema])
validateCards edition border cards = (failures, successes)
  where
  (failures, successes) = partitionEithers (fmap go cards)
  
  go :: CardObject -> Either CardObject CardSchema
  go c = c &
    ( validateCard edition border
    > maybe2either c
    )

{-|

@
successes <- 'validateCardsM' edition border cards
@

-}
validateCardsM
  :: (MonadThrow m)
  => EditionName
  -> Border 
  -> [CardObject]
  -> m [CardSchema]
validateCardsM edition border cards = do

  case failures of
    
    [] -> return successes
    
    cs -> failWith cs
  
  where
  (failures, successes) = go cards

  go = validateCards edition border

  failWith cs = throwS message
    where
    es = cs <&> _CardObject_name :: [Text]
    sItems  = show es
    sTotal  = show $ length es
    message =
            "[validateCardsM] these cards (" <> sTotal <> "total) were invalid: " <> sItems

----------------------------------------

{-|




-}
validateCard
  :: EditionName -> Border 
  -> CardObject -> Maybe CardSchema
validateCard (_CardSchema_edition) (_CardSchema_border) CardObject{..} = do

  (MultiverseID -> _CardSchema_multiverseid) <- _CardObject_multiverseid

  (CollectorNumber -> _CardSchema_ccn) <-_CardObject_number

  _CardSchema_numeric <- validateNumeric
    (_CardObject_power, _CardObject_toughness)
    _CardObject_loyalty
    (_CardObject_hand, _CardObject_life)

  _CardSchema_foreignNames  <- _CardObject_foreignNames
    & fromMaybe []
    & traverse toForeignPrinting

  _CardSchema_legalities    <- _CardObject_legalities
    & fromMaybe []
    & traverse toFormatLegality

  _CardSchema_rulings       <- _CardObject_rulings
    & fromMaybe []
    & traverse toRuling
  
  pure$ CardSchema{..}

  where
  _CardSchema_identity      = _CardObject_id   & UniqueID
  _CardSchema_name          = _CardObject_name & CardName

  _CardSchema_mciNumber     = _CardObject_mciNumber

  _CardSchema_layout        = _CardObject_layout & toLayout
  _CardSchema_names         = _CardObject_names
    & maybe [] (fmap CardName)

  _CardSchema_variations    = _CardObject_variations
    & maybe [] (fmap MultiverseID)
  -- _CardSchema_border        = _CardObject_border & toBorder 

  _CardSchema_cmc           = _CardObject_cmc           & id
  _CardSchema_manaCost      = _CardObject_manaCost      & fmap ManaCost --
  _CardSchema_colors        = _CardObject_colors        & toColors
  _CardSchema_colorIdentity = _CardObject_colorIdentity & toColors
  
  _CardSchema_supertypes    = _CardObject_supertypes & maybe [] (fmap Supertype)
  _CardSchema_cardtypes     = _CardObject_types      & maybe [] (fmap Cardtype)
  _CardSchema_subtypes      = _CardObject_subtypes   & maybe [] (fmap Subtype)

  _CardSchema_oracle        = _CardObject_text & toOracle

  _CardSchema_rarity        = _CardObject_rarity & Rarity
  _CardSchema_flavor        = _CardObject_flavor & maybe def Flavor
  _CardSchema_artist        = _CardObject_artist & Artist
  
  _CardSchema_originalText  = _CardObject_originalText
  _CardSchema_originalType  = _CardObject_originalType
   
  _CardSchema_reserved      = _CardObject_reserved    & maybe def isReserved
  _CardSchema_starter       = _CardObject_starter     & maybe def isStarter
  _CardSchema_timeshifted   = _CardObject_timeshifted & maybe def isTimeshifted

  _CardSchema_printings     = _CardObject_printings
    & maybe [] (fmap EditionName)
  
----------------------------------------

toForeignPrinting :: CardForeignPrintingObject -> Maybe ForeignPrinting
toForeignPrinting CardForeignPrintingObject{..} = do
  (MultiverseID -> _Foreign_multiverseid) <- _CardForeignPrintingObject_multiverseid 
  
  pure ForeignPrinting{..}
  where
  _Foreign_language     = _CardForeignPrintingObject_language & Language
  _Foreign_name         = _CardForeignPrintingObject_name     & CardName

toFormatLegality :: CardFormatLegalityObject -> Maybe FormatLegality
toFormatLegality CardFormatLegalityObject{..} = do
  pure FormatLegality{..}
  where
  _FormatLegality_format   = _CardFormatLegalityObject_format   & Format
  _FormatLegality_legality = _CardFormatLegalityObject_legality & Legality
  
toRuling :: CardRulingObject -> Maybe Ruling
toRuling CardRulingObject{..} = do
  _Ruling_date <- parseDay _CardRulingObject_date
  
  pure$ Ruling{..}
  where
  _Ruling_text = _CardRulingObject_text & id

----------------------------------------

validateNumeric
  :: (Maybe Text, Maybe Text)
  -> Maybe Natural
  -> (Maybe Integer, Maybe Integer)
  -> Maybe NumericSchema
-- validateNumeric (power,toughness) loyalty (hand,life) = do

validateNumeric (Just power, Just toughness) Nothing (Nothing,Nothing)
  = Just $ CreatureSchema CreatureLike{..}
  where
  _Creature_power     = parseNumberLike power
  _Creature_toughness = parseNumberLike toughness

validateNumeric (Nothing,Nothing) (Just loyalty) (Nothing,Nothing)
  = Just $ PlaneswalkerSchema (PlaneswalkerLike loyalty)
    
validateNumeric (Nothing,Nothing) Nothing (Just hand, Just life)
  = Just $ VanguardSchema VanguardLike{..}
  where
  _Vanguard_hand = hand
  _Vanguard_life = life

validateNumeric _ _ _ = Nothing

{-

  | CreatureSchema     CreatureLike
  | PlaneswalkerSchema PlaneswalkerLike
  | VanguardSchema     VanguardLike

-}

----------------------------------------
 
{-|

formatting:

@
YYYY-MM-DD or YYYY-MM or YYYY
@

examples:

@
"2010-07-22" or "2010-07" or "2010"
@

>>> :set -XOverloadedStrings
>>> parseDay "2010-07-22"
Just 2010-07-22
>>> parseDay "2010-07"
Just 2010-07-01
>>> parseDay "2010"
Just 2010-01-01
>>> parseDay "  2010-07-22  "
Just 2010-07-22

-}
parseDay :: Text -> Maybe Day
parseDay = getFirst . parseDayViaFormat
  [ "%Y-%m-%d"
  , "%Y-%m"
  , "%Y"
  ]

parseDayViaFormat :: [String] -> Text -> First Day
parseDayViaFormat acceptableTimeFormats t = firstValidParse
  where
  firstValidParse
    = mconcat allParses
  allParses
    = parseVia <$> acceptableTimeFormats  
  parseVia aTimeFormat
    = parseTime defaultTimeLocale aTimeFormat (toS t)
    & First

-- parseDay = toS > parse
--   where
--   parse = Attoparsec.parseOnly p > either2maybe
--   p = timeParser defaultTimeLocale "YYYY-MM-DD"
--
--parseDay = toS > readMay
--
--parseDay = toS > parseTime defaultTimeLocale "YYYY-MM-DD"
-- T.unpack

{-NOTES

timeParser :: TimeLocale -> String -> Parser TimeParse

parseTime :: (ParseTime t) => TimeLocale -> String -> String -> Maybe t
parseTime l spec = either (const Nothing) Just
        . P.parseOnly parser . utf8String where
    parser = buildTime <$ P.skipSpace <*> timeParser l spec
        <* P.skipSpace <* P.endOfInput

-}

----------------------------------------