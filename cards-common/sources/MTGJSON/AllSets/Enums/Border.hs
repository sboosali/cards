{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|


-}
module MTGJSON.AllSets.Enums.Border where

import MTGJSON.Extra

import Control.Lens (makePrisms)

----------------------------------------

newtype Border = Border Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''Border

-- | @= 'blackBorder'@
instance Default Border where def = blackBorder

----------------------------------------

toBorder :: Maybe Text -> Border
toBorder = maybe def Border

----------------------------------------

blackBorder :: Border
blackBorder = "black"

whiteBorder :: Border
whiteBorder = "white"

silverBorder :: Border
silverBorder = "silver"

----------------------------------------
