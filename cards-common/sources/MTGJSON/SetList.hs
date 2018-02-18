{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.SetList where

import MTGJSON.Extra
import MTGJSON.Aeson (parseJSON_TypePrefix)

import Data.Time.Calendar (Day(..))

import Data.Aeson.Types (FromJSON(..))
  
----------------------------------------

-- | @= 'eitherDecode' @ 
pSetsMetadata :: ByteString -> Either String SetsMetadata
pSetsMetadata = eitherDecode
  
----------------------------------------

{-|

-}
data SetsMetadata = SetsMetadata [SetMetadata]
 deriving (Show,Read,Eq,Ord,Data,Generic,NFData,FromJSON) --,Hashable)

--instance FromJSON SetsMetadata 

----------------------------------------

{-|

e.g.

@
  {
    "name":        "Fifth Dawn",
    "code":        "5DN",
    "releaseDate": "2004-06-04",
    "block":       "Mirrodin"
  }
@

-}
data SetMetadata = SetMetadata
 { _SetMetadata_name        :: Text
 , _SetMetadata_code        :: Text
 , _SetMetadata_releaseDate :: Day
 , _SetMetadata_block       :: Maybe Text
 } deriving (Show,Read,Eq,Ord,Data,Generic,NFData)--,Hashable)

instance FromJSON SetMetadata where
  parseJSON = parseJSON_TypePrefix

----------------------------------------

{-NOTES

> import Data.Time.Calendar
> let day = fromGregorian 2004 06 04
> toModifiedJulianDay (day
53160
> showGregorian day
"2004-06-04"
> day
2004-06-04

-}
