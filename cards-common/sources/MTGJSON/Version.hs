{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.Version where 

import MTGJSON.Extra
--import MTGJSON.Aeson (parseJSON_TypePrefix)

import Data.Aeson.Types (FromJSON(..),Value(..),typeMismatch)
import Data.Aeson       --(eitherDecode) 

import "base" Data.Version (Version)
--import "base" Text.ParserCombinators.ReadP (readP_to_S)

--import Prelude.Spiros

----------------------------------------

-- | @= 'eitherDecode' @ 
pVersionObject :: ByteString -> Either String VersionObject
pVersionObject = eitherDecode

----------------------------------------

{-| Parse the version as either an object or a string.

e.g.

@
{
  "version": "3.13.1"
}
@

e.g.

@
"3.13.1"
@

-}
data VersionObject = VersionObject
 { _VersionObject_version :: Version
 } deriving (Show,Read,Eq,Ord,Data,Generic,NFData,Hashable)

instance FromJSON VersionObject where
 parseJSON (Object o) = VersionObject
   <$> o .: "version"
 parseJSON (String s) = VersionObject
   <$> parseJSON (String s)
 parseJSON invalid    = typeMismatch "VersionObject" invalid

-- pVersion :: String -> Maybe Version
-- pVersion
--   = readP_to_S parseVersion
--   > fmap fst > safeHead

----------------------------------------

{-NOTES

> eitherDecode "\"3.13.1\"" :: Either String Version
Right (Version {versionBranch = [3,13,1], versionTags = []})

> pVersionObject  "\"3.13.1\"" 
Right (VersionObject {_VersionObject_version = Version {versionBranch = [3,13,1], versionTags = []}})
> pVersionObject  "{\n\"version\": \"3.13.1\"\n}" 
Right (VersionObject {_VersionObject_version = Version {versionBranch = [3,13,1], versionTags = []}})

-}
