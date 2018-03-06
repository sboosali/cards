{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

Languages that are officially supported:

* 'english'
* 'german'
* 'french'
* 'italian'
* 'spanish'
* 'portuguese'
* 'japanese'
* 'chinese'
* 'russian'
* 'taiwanese'
* 'korean'

-}
module MTGJSON.AllSets.Enums.Language where

import MTGJSON.Extra

import Control.Lens (makeLenses, makePrisms)

----------------------------------------

newtype Language = Language Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

-- | @= 'english'@
instance Default Language where def = english

----------------------------------------

data LanguageInfo = LanguageInfo
 { _languageAbbreviation :: Text
 , _languageEndonym      :: Text
 } deriving (Show,Read,Eq,Ord,Generic)

instance NFData   LanguageInfo
instance Hashable LanguageInfo

-- | @= 'englishInfo'@
instance Default LanguageInfo where def = englishInfo

----------------------------------------

knownLanguages :: Set Language
knownLanguages =
  [ english
  , german
  , french
  , italian
  , spanish
  , portuguese
  , japanese
  , chinese
  , russian
  , taiwanese
  , korean
  ]

----------------------------------------

-- | 'englishAbbreviation' and 'englishEndonym'. 
englishInfo :: LanguageInfo
englishInfo = LanguageInfo englishAbbreviation englishEndonym

-- | 'germanAbbreviation' and 'germanEndonym'. 
germanInfo :: LanguageInfo
germanInfo = LanguageInfo germanAbbreviation germanEndonym

-- | 'frenchAbbreviation' and 'frenchEndonym'. 
frenchInfo :: LanguageInfo
frenchInfo = LanguageInfo frenchAbbreviation frenchEndonym

-- | 'italianAbbreviation' and 'italianEndonym'. 
italianInfo :: LanguageInfo
italianInfo = LanguageInfo italianAbbreviation italianEndonym

-- | 'spanishAbbreviation' and 'spanishEndonym'. 
spanishInfo :: LanguageInfo
spanishInfo = LanguageInfo spanishAbbreviation spanishEndonym

-- | 'portugueseAbbreviation' and 'portugueseEndonym'. 
portugueseInfo :: LanguageInfo
portugueseInfo = LanguageInfo portugueseAbbreviation portugueseEndonym

-- | 'japaneseAbbreviation' and 'japaneseEndonym'. 
japaneseInfo :: LanguageInfo
japaneseInfo = LanguageInfo japaneseAbbreviation japaneseEndonym

-- | 'chineseAbbreviation' and 'chineseEndonym'. 
chineseInfo :: LanguageInfo
chineseInfo = LanguageInfo chineseAbbreviation chineseEndonym

-- | 'russianAbbreviation' and 'russianEndonym'. 
russianInfo :: LanguageInfo
russianInfo = LanguageInfo russianAbbreviation russianEndonym

-- | 'taiwaneseAbbreviation' and 'taiwaneseEndonym'. 
taiwaneseInfo :: LanguageInfo
taiwaneseInfo = LanguageInfo taiwaneseAbbreviation taiwaneseEndonym

-- | 'koreanAbbreviation' and 'koreanEndonym'. 
koreanInfo :: LanguageInfo
koreanInfo = LanguageInfo koreanAbbreviation koreanEndonym

----------------------------------------

english :: Language
english = "English"

german :: Language
german = "German"

french :: Language
french = "French"

italian :: Language
italian = "Italian"

spanish :: Language
spanish = "Spanish"

portuguese :: Language
portuguese = "Portuguese"

japanese :: Language
japanese = "Japanese"

chinese :: Language
chinese = "Chinese"

russian :: Language
russian = "Russian"

taiwanese :: Language
taiwanese = "Taiwanese"

korean :: Language
korean = "Korean"

----------------------------------------

englishAbbreviation :: Text
englishAbbreviation = "en"

germanAbbreviation :: Text
germanAbbreviation = "de"

frenchAbbreviation :: Text
frenchAbbreviation = "fr"

italianAbbreviation :: Text
italianAbbreviation = "it"

spanishAbbreviation :: Text
spanishAbbreviation = "es"

portugueseAbbreviation :: Text
portugueseAbbreviation = "pt"

japaneseAbbreviation :: Text
japaneseAbbreviation = "jp"

chineseAbbreviation :: Text
chineseAbbreviation = "cn"

russianAbbreviation :: Text
russianAbbreviation = "ru"

taiwaneseAbbreviation :: Text
taiwaneseAbbreviation = "tw"

koreanAbbreviation :: Text
koreanAbbreviation = "ko"

----------------------------------------

englishEndonym :: Text
englishEndonym = "English"

germanEndonym :: Text
germanEndonym = "Deutsch"

frenchEndonym :: Text
frenchEndonym = "Français"

italianEndonym :: Text
italianEndonym = "Italiano"

spanishEndonym :: Text
spanishEndonym = "Español"

portugueseEndonym :: Text
portugueseEndonym = "Português"

japaneseEndonym :: Text
japaneseEndonym = "日本語"

chineseEndonym :: Text
chineseEndonym = "简体中文"

russianEndonym :: Text
russianEndonym = "Русский"

taiwaneseEndonym :: Text
taiwaneseEndonym = "繁體中文"

koreanEndonym :: Text
koreanEndonym = "한국어"

----------------------------------------

{-
languageInfo :: (MonadThrow m) => Language -> m LanguageInfo
languageInfo = \case  
 Language "English"    -> return $ LanguageInfo "en" "English"
 Language "German"     -> return $ LanguageInfo "de" "Deutsch"
 Language "French"     -> return $ LanguageInfo "fr" "Français"
 Language "Italian"    -> return $ LanguageInfo "it" "Italiano"
 Language "Spanish"    -> return $ LanguageInfo "es" "Español"
 Language "Portuguese" -> return $ LanguageInfo "pt" "Português"
 Language "Japanese"   -> return $ LanguageInfo "jp" "日本語"
 Language "Chinese"    -> return $ LanguageInfo "cn" "简体中文"
 Language "Russian"    -> return $ LanguageInfo "ru" "Русский"
 Language "Taiwanese"  -> return $ LanguageInfo "tw" "繁體中文"
 Language "Korean"     -> return $ LanguageInfo "ko" "한국어"
 Language language     -> throwT $ "unknown language " <> language
-}

----------------------------------------

makePrisms ''Language

makeLenses ''LanguageInfo

----------------------------------------
