{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.Known
 ( module MTGJSON.Known
 , module MTGJSON.Known.Types
 , module MTGJSON.Known.Newtypes
 ) where

-- re-exports
import MTGJSON.Known.Types
import MTGJSON.Known.Newtypes

--import MTGJSON.Extra
import MTGJSON.Types
import MTGJSON.Kinds

----------------------------------------

newtype KnownCard = KnownCard
 (Card Known)

----------------------------------------

newtype Known (u :: CHARACTERISTIC)
  = Known (KnownType u)

----------------------------------------  

type family KnownType (u :: CHARACTERISTIC) where

 KnownType CARD           = Name

 KnownType UNIQUE         = UniqueIdentifier
 KnownType MULTIVERSEID   = MultiverseIdentifier
 KnownType NAME           = Name
 KnownType ORACLE         = Oracle

 KnownType COLOR          = Color {-KnownColor-}
 KnownType CHROMA         = Chroma {-KnownChroma-}

-- KnownType MANA           = Mana {-KnownMana-}

 KnownType COST           = KnownCost {-Cost-}

 KnownType NUMERIC        = KnownNumeric {-Numeric-}

 KnownType SUPERTYPE      = KnownSupertype {-Supertype-}
 KnownType TYPE           = KnownBaseType {-BaseType-}
 KnownType SUBTYPE        = KnownSubtype {-Subtype-}

 KnownType FACE           = KnownFace {-KnownFace-}
 --TODOKnownType LAYOUT         = KnownLayout

 KnownType RARITY         = KnownRarity
 KnownType WATERMARK      = KnownWatermark

 KnownType EDITION        = KnownEdition
 KnownType BLOCK          = KnownBlock

 KnownType LEGALITY       = KnownLegality
 KnownType FORMAT         = KnownFormat

 KnownType LANGUAGE       = KnownLanguage

----------------------------------------

{-

 KnownType UNIQUE         = 
 KnownType NAME           = 
 KnownType MANACOST       = 
 KnownType COLOR          = 

 KnownType ORACLE         = 
 KnownType NUMBER         = 
 KnownType CMC            = 
 KnownType COLORIDENTITY  = 
  
 KnownType SUPERTYPE      = 
 KnownType TYPE           = 
 KnownType SUBTYPE        = 
  
 KnownType LAYOUT         = 
 KnownType WATERMARK      = 
 KnownType RARITY         = 
 KnownType EDITION        = 
 KnownType FLAVOR         = 
 KnownType ARTIST         = 
 KnownType COLLECTORNUMBER= 
 KnownType MULTIVERSEID   = 

 KnownType RULING         = 
 KnownType LEGALITY       = 

 KnownType VARIATION      = 
 KnownType PRINTING       = 
 KnownType FOREIGNVARIATION= 

 KnownType LANGUAGE       = 
 KnownType FORMAT         = 

 KnownType TEXT           = 
 KnownType DATE           = 

-}

----------------------------------------
