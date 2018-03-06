
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds, ScopedTypeVariables #-}

{-|

-}
module MTGJSON.AllSets.Schema where

  -- ( module Schema
  -- ) where

import MTGJSON.AllSets.Object as Object
import MTGJSON.AllSets.Set    as Edition
import MTGJSON.AllSets.Card   as Card

import "thyme"      Data.Thyme
import "old-locale" System.Locale (defaultTimeLocale)
--import qualified "attoparsec" Data.Attoparsec.ByteString as Attoparsec

import Data.Monoid
import Prelude.Spiros

----------------------------------------

strictlyValidateEditions :: [SetObject] -> Maybe Editions
strictlyValidateEditions 
 = traverse validateEdition
 > fmap Editions

{-|

@
(invalids, valids) = validateEdition _
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

validateEdition :: SetObject -> Maybe Edition
validateEdition _SetObject@SetObject{..} = do

 -- _Edition_releaseDate <- _SetObject_releaseDate <&> parseDay
  releaseDate          <- _SetObject_releaseDate
  _Edition_releaseDate <- parseDay releaseDate 

  block                <- _SetObject_block 
  let _Edition_block = BlockName block
  
  pure$ Edition{..}
 
 where
 _Edition_name        = _SetObject_name         &  EditionName
 _Edition_codes       = _SetObject              &  getEditionCodes
 _Edition_type        = _SetObject_type         &  EditionType
 _Edition_border      = _SetObject_border       &  maybe blackBorder Border
 _Edition_booster     = _SetObject_booster      &  maybe defaultBooster toUniformBooster
 -- _Edition_releaseDate = _SetObject_releaseDate <&> (parseDay > join)
 _Edition_onlineOnly  = _SetObject_onlineOnly   &  maybe OfflineToo fromOnlineOnly

-- validateEdition :: SetObject -> Edition
-- validateEdition _SetObject@SetObject{..} = Edition{..}
 
----------------------------------------

-- validateCard :: CardObject -> Maybe CardSchema
-- validateCard CardObject{..} = _

----------------------------------------

--fromMaybe ""

