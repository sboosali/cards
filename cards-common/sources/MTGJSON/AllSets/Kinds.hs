{-# LANGUAGE DataKinds #-}

{-|

-}
module MTGJSON.AllSets.Kinds where

----------------------------------------

-- data Card = Card
--  {
--  }

data CHARACTERISTIC
 = IDENTIFIER      -- ^ 
 | NAME            -- ^ 
 | MANACOST        -- ^ 
 | COLOR           -- ^ 

 | ORACLE          -- ^ 
 | NUMBER          -- ^ 
 | CMC             -- ^ 
 | COLORIDENTITY   -- ^ 
  
 | SUPERTYPE       -- ^ 
 | TYPE            -- ^ 
 | SUBTYPE         -- ^ 
  
 | LAYOUT          -- ^ 
 | WATERMARK       -- ^ 
 | RARITY          -- ^ 
 | FLAVOR          -- ^ 
 | ARTIST          -- ^ 
 | COLLECTORNUMBER -- ^ 
 | MULTIVERSEID    -- ^ 

 | RULING          -- ^ 
 | LEGALITY        -- ^ 

 | VARIATION       -- ^ 
 | PRINTING        -- ^ 
 | FOREIGNVARIATION -- ^ 

 | LANGUAGE        -- ^ 
 | FORMAT          -- ^ 

 | TEXT            -- ^
 | DATE            -- ^
 
 --TODO  | DATE  | TEXT

----------------------------------------

{-


data CHARACTERISTIC
 = IDENTIFIER      -- ^ 
 | NAME            -- ^ 
 | MANACOST        -- ^ 
 | COLORS          -- ^ 
 | TYPE            -- ^ 
 | ORACLE          -- ^ 
 | NUMBER          -- ^ 
 | CMC             -- ^ 
 | COLORIDENTITY   -- ^ 
 | NAMES           -- ^ 
  
 | SUPERTYPES      -- ^ 
 | TYPES           -- ^ 
 | SUBTYPES        -- ^ 
  
 | LAYOUT          -- ^ 
 | WATERMARK       -- ^ 
 | RARITY          -- ^ 
 | FLAVOR          -- ^ 
 | ARTIST          -- ^ 
 | COLLECTORNUMBER -- ^ 
 | MULTIVERSEID    -- ^ 
 | MCINUMBER       -- ^ 

 | RULINGS         -- ^ 
 | LEGALITIES      -- ^ 

 | VARIATIONS      -- ^ 
 | PRINTINGS       -- ^ 
 | ORIGINALTEXT    -- ^ 
 | ORIGINALTYPE    -- ^ 
 | FOREIGNNAMES    -- ^ 

 | LANGUAGE        -- ^ 
 | FORMAT          -- ^ 
 | LEGALITY        -- ^

 | TEXT            -- ^
 | DATE            -- ^






= ID            :: CardId 
 | NAME          :: CardName 
 | MANACOST      :: Maybe ManaCost 
 | COLORS        :: [CardColor] 
 | TYPE          :: CardTypeLine 
 | ORACLE        :: OracleText 
 | NUMBER        ::   
 | CMC           :: ConvertedManaCost 
 | COLORIDENTITY :: [CardColorIdentity] 
 | NAMES         :: Maybe [CardName]
  
 | SUPERTYPES    :: [CardSupertype] 
 | TYPES         :: (NonEmpty CardTypes) 
 | SUBTYPES      :: [CardSubtype]
  
 | LAYOUT        :: CardLayout 
 | WATERMARK     :: Maybe Text 
 | RARITY        :: Probably KnownCardRarity 
 | FLAVOR        :: CardFlavorText 
 | ARTIST        :: CardArtist
 | COLLECTORNUMBER      :: CardCollectorNumber 
 | MULTIVERSEID  :: WizardsIdentifier 
 | MCINUMBER     :: CardCollectorNumber

 | RULINGS       :: [CardRuling] 
 | LEGALITIES    :: [CardFormatLegality]

 | VARIATIONS    :: [WizardsIdentifier] 
 | PRINTINGS     :: [CardSetCode] 
 | ORIGINALTEXT  :: Maybe CardText 
 | ORIGINALTYPE  :: Maybe CardTypeLine 
 | FOREIGNNAMES  :: [CardForeignPrinting] 
-}
