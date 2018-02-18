{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.Types where

--import MTGJSON.Extra

import Prelude.Spiros

----------------------------------------

{-| loosens a type. 

for example, if the "known type" @a@ is an Enum, this
"adds" infinitely many "unknown constructors" as strings.

e.g. 

-}
data Probably a
  = Unknown String
  | Known   a
  deriving (Functor,Show,Read,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------
