{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTGJSON.AllSets.Enums.Legality where

import MTGJSON.Extra

import Control.Lens (makePrisms)

----------------------------------------

newtype Legality = Legality Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''Legality

-- | @= 'legal'@
instance Default Legality where def = legal

----------------------------------------

legal :: Legality
legal = "legal"

restricted :: Legality
restricted = "restricted"

banned :: Legality
banned = "banned"

----------------------------------------
