{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|


-}
module MTGJSON.AllSets.Oracle where

import MTGJSON.Extra

import Control.Lens (makePrisms)

----------------------------------------

{-| 
-}
type SimpleOracle = Oracle Text

----------------------------------------

{-| parametrized over a token type @t@. 

-}
newtype Oracle t = Oracle
  { getOracle :: [OracleParagraph t]
  }
  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

-- | @= 'vanilla'@
instance Default (Oracle t) where def = vanilla

-- | 
instance IsList (Oracle t) where
  type Item (Oracle t) = (OracleParagraph t)
  toList   = getOracle
  fromList = Oracle

-- instance NFData     (Oracle t)
-- instance Hashable   (Oracle t)

----------------------------------------

{-| parametrized over a token type @t@. 

-}
newtype OracleParagraph t = OracleParagraph
  { getOracleParagraph :: [t]
  }
  deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)

-- | 
instance IsList (OracleParagraph t) where
  type Item (OracleParagraph t) = t
  toList   = getOracleParagraph
  fromList = OracleParagraph

----------------------------------------

{-| A vanilla card is one with no oracle
(i.e. non-reminder) text.

@
= 'Oracle' []
@

-}
vanilla :: Oracle t
vanilla = []

----------------------------------------

makeLenses ''Oracle

makePrisms ''OracleParagraph

----------------------------------------
