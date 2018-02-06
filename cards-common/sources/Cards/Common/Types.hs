{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

{-|

-}
module Cards.Common.Types where

import Cards.Common.Extra

----------------------------------------

{-| magiccards.info

-}
data MCICardIdentifier = MCICardIdentifier 
  { cardLanguage        :: Language  -- TODO 
  , cardSet             :: MagicSetCode 
  , cardCollectorNumber :: CollectorNumber  
  }

type Language = Text

type MagicSetCode = Text 

type CollectorNumber = Text  
-- type CollectorNumber = Natural 

----------------------------------------

defaultMCICardIdentifier
  :: MagicSetCode -> CollectorNumber -> MCICardIdentifier 
defaultMCICardIdentifier cardSet cardCollectorNumber
  = MCICardIdentifier{..}
  where 
  cardLanguage = "en"

----------------------------------------
