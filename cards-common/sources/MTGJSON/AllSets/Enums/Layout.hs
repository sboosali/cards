{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|



-}
module MTGJSON.AllSets.Enums.Layout where

import MTGJSON.Extra

import Control.Lens (makePrisms)

----------------------------------------

newtype Layout = Layout Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''Layout
----------------------------------------

{-|
-}
aftermathLayout :: Layout
aftermathLayout = "aftermath"

{-|
-}
doubleFacedLayout :: Layout
doubleFacedLayout = "double-faced"

{-|
-}
flipLayout :: Layout
flipLayout = "flip"

{-|
-}
levelerLayout :: Layout
levelerLayout = "leveler"

{-|
-}
meldLayout :: Layout
meldLayout = "meld"

{-|
-}
normalLayout :: Layout
normalLayout = "normal"

{-|
-}
planeLayout :: Layout
planeLayout = "plane"

{-|
-}
phenomenonLayout :: Layout
phenomenonLayout = "phenomenon"

{-|
-}
schemeLayout :: Layout
schemeLayout = "scheme"

{-|
-}
splitLayout :: Layout
splitLayout = "split"

{-|
-}
tokenLayout :: Layout
tokenLayout = "token"

{-|
-}
vanguardLayout :: Layout
vanguardLayout = "vanguard"

----------------------------------------

{-

-}

