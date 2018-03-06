{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTGJSON.AllSets.Enums.Name where

import MTGJSON.Extra

import Control.Lens (makePrisms)

----------------------------------------

newtype CardName = CardName Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''CardName

----------------------------------------

----------------------------------------
