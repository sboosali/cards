
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTGJSON.AllSets.Enums.Edition where

import MTGJSON.Extra
import MTGJSON.AllSets.Enums.Language

import Control.Lens (makeLenses, makePrisms)

----------------------------------------

newtype EditionName = EditionName Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''EditionName

newtype EditionCode = EditionCode Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''EditionCode

newtype EditionType = EditionType Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''EditionType

----------------------------------------

data QualifiedEdition = QualifiedEdition
 { _editionEdition  :: EditionName
 , _editionLanguage :: Maybe Language
 } deriving (Show,Read,Eq,Ord,Generic)

instance NFData   QualifiedEdition
instance Hashable QualifiedEdition

makeLenses ''QualifiedEdition
  
----------------------------------------

data EditionInfo = EditionInfo
 { _editionAbbreviation :: Text
 , _editionDescription  :: Text
 --, _editionLanguages    :: [Language] --NOTE a `Set` 
 } deriving (Show,Read,Eq,Ord,Generic)

instance NFData   EditionInfo
instance Hashable EditionInfo

makeLenses ''EditionInfo

----------------------------------------

coreEdition :: EditionType
coreEdition = "core"

expansionEdition :: EditionType
expansionEdition = "expansion"

reprintEdition :: EditionType
reprintEdition = "reprint"

boxEdition :: EditionType
boxEdition = "box"

unEdition :: EditionType
unEdition = "un"

vaultEdition :: EditionType
vaultEdition = "from the vault"

premiumEdition :: EditionType
premiumEdition = "premium deck"

duelEdition :: EditionType
duelEdition = "duel deck"

starterEdition :: EditionType
starterEdition = "starter"

commanderEdition :: EditionType
commanderEdition = "commander"

planechaseEdition :: EditionType
planechaseEdition = "planechase"

archenemyEdition :: EditionType
archenemyEdition = "archenemy"

promoEdition :: EditionType
promoEdition = "promo"

vanguardEdition :: EditionType
vanguardEdition = "vanguard"

mastersEdition :: EditionType
mastersEdition = "masters"

conspiracyEdition :: EditionType
conspiracyEdition = "conspiracy"

masterpieceEdition :: EditionType
masterpieceEdition = "masterpiece"

----------------------------------------

----------------------------------------

eAL :: EditionCode
eAL = "AL"

eBE :: EditionCode
eBE = "BE"

eUN :: EditionCode
eUN = "UN"

eRV :: EditionCode
eRV = "RV"

eSUMMER :: EditionCode
eSUMMER = "SUMMER"

eE4 :: EditionCode
eE4 = "E4"

eE5 :: EditionCode
eE5 = "E5"

eE6 :: EditionCode
eE6 = "E6"

eE7 :: EditionCode
eE7 = "E7"

eE8 :: EditionCode
eE8 = "E8"

eE9 :: EditionCode
eE9 = "E9"

eE10 :: EditionCode
eE10 = "E10"

eM10 :: EditionCode
eM10 = "M10"

eM11 :: EditionCode
eM11 = "M11"

eM12 :: EditionCode
eM12 = "M12"

eM13 :: EditionCode
eM13 = "M13"

eM14 :: EditionCode
eM14 = "M14"

eM15 :: EditionCode
eM15 = "M15"

eORI :: EditionCode
eORI = "ORI"

eAN :: EditionCode
eAN = "AN"

eAQ :: EditionCode
eAQ = "AQ"

eLG :: EditionCode
eLG = "LG"

eDK :: EditionCode
eDK = "DK"

eFE :: EditionCode
eFE = "FE"

eHL :: EditionCode
eHL = "HL"

eMR :: EditionCode
eMR = "MR"

eVI :: EditionCode
eVI = "VI"

eWL :: EditionCode
eWL = "WL"

eTP :: EditionCode
eTP = "TP"

eSH :: EditionCode
eSH = "SH"

eEX :: EditionCode
eEX = "EX"

eUS :: EditionCode
eUS = "US"

eUL :: EditionCode
eUL = "UL"

eUD :: EditionCode
eUD = "UD"

eMM :: EditionCode
eMM = "MM"

eNE :: EditionCode
eNE = "NE"

ePR :: EditionCode
ePR = "PR"

eIN :: EditionCode
eIN = "IN"

ePS :: EditionCode
ePS = "PS"

eAP :: EditionCode
eAP = "AP"

eOD :: EditionCode
eOD = "OD"

eTR :: EditionCode
eTR = "TR"

eJU :: EditionCode
eJU = "JU"

eON :: EditionCode
eON = "ON"

eLE :: EditionCode
eLE = "LE"

eSC :: EditionCode
eSC = "SC"

eMI :: EditionCode
eMI = "MI"

eDS :: EditionCode
eDS = "DS"

eDN5 :: EditionCode
eDN5 = "DN5"

eCHK :: EditionCode
eCHK = "CHK"

eBOK :: EditionCode
eBOK = "BOK"

eSOK :: EditionCode
eSOK = "SOK"

eRAV :: EditionCode
eRAV = "RAV"

eGP :: EditionCode
eGP = "GP"

eDI :: EditionCode
eDI = "DI"

eIA :: EditionCode
eIA = "IA"

eAI :: EditionCode
eAI = "AI"

eCS :: EditionCode
eCS = "CS"

eTSTS :: EditionCode
eTSTS = "TSTS"

eTS :: EditionCode
eTS = "TS"

ePC :: EditionCode
ePC = "PC"

eFUT :: EditionCode
eFUT = "FUT"

eLW :: EditionCode
eLW = "LW"

eMT :: EditionCode
eMT = "MT"

eSHM :: EditionCode
eSHM = "SHM"

eEVE :: EditionCode
eEVE = "EVE"

eALA :: EditionCode
eALA = "ALA"

eCFX :: EditionCode
eCFX = "CFX"

eARB :: EditionCode
eARB = "ARB"

eZEN :: EditionCode
eZEN = "ZEN"

eWWK :: EditionCode
eWWK = "WWK"

eROE :: EditionCode
eROE = "ROE"

eSOM :: EditionCode
eSOM = "SOM"

eMBS :: EditionCode
eMBS = "MBS"

eNPH :: EditionCode
eNPH = "NPH"

eISD :: EditionCode
eISD = "ISD"

eDKA :: EditionCode
eDKA = "DKA"

eAVR :: EditionCode
eAVR = "AVR"

eRTR :: EditionCode
eRTR = "RTR"

eGTC :: EditionCode
eGTC = "GTC"

eDGM :: EditionCode
eDGM = "DGM"

eTHS :: EditionCode
eTHS = "THS"

eBNG :: EditionCode
eBNG = "BNG"

eJOU :: EditionCode
eJOU = "JOU"

eKTK :: EditionCode
eKTK = "KTK"

eFRF :: EditionCode
eFRF = "FRF"

eDTK :: EditionCode
eDTK = "DTK"

eBFZ :: EditionCode
eBFZ = "BFZ"

eOGW :: EditionCode
eOGW = "OGW"

eSOI :: EditionCode
eSOI = "SOI"

eEMN :: EditionCode
eEMN = "EMN"

eKLD :: EditionCode
eKLD = "KLD"

eAER :: EditionCode
eAER = "AER"

eAKH :: EditionCode
eAKH = "AKH"

eHOU :: EditionCode
eHOU = "HOU"

eXLN :: EditionCode
eXLN = "XLN"

eRIX :: EditionCode
eRIX = "RIX"

----------------------------------------
