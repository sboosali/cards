{-# LANGUAGE CPP #-}

{-# LANGUAGE OverloadedStrings #-}

{-|

transitively imports @Paths_cards_common@. 

-}
module Cards.MTGJSON.ByteStringLiterals where

import Data.ByteString (ByteString)

----------------------------------------

{-

*Cards.Common Cards.MTGJSON.Schema Cards.MTGJSON.Paths B8 B Cards.MTGJSON.Macros> 
writeFile "ignore/RIXSetsY.txt" (show b'RIXSetsY)

-}

b'RIXSetsY :: ByteString
b'RIXSetsY = ""
