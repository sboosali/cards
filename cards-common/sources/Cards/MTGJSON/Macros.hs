{-TODO-}

{-# LANGUAGE TemplateHaskell #-}

{-|

transitively imports @Paths_cards_common@. 

-}
module Cards.MTGJSON.Macros where

--import Cards.MTGJSON.Paths

--import Data.ByteString (ByteString)

--import Prelude.Spiros

----------------------------------------

-- {-

-- *Cards.Common Cards.MTGJSON.Schema Cards.MTGJSON.Paths B8 B Cards.MTGJSON.Macros> 
-- writeFile "ignore/RIXSetsY.txt" (show b'RIXSetsY)

-- -}

-- b'RIXSetsY :: ByteString
-- b'RIXSetsY = ""

----------------------------------------

-- b'RIXSetsY :: ByteString
-- b'RIXSetsY = $(embedDataFile fp'RIXSetsY)

{-

embedDataFile

    runNodeInteractive
    haddock: internal error: /nix/store/lfwjghzhfmj6n20qma02w35ydm6xcjkd-cards-common-0.0.0-data/share/ghcjs-0.2.0/x86_64-linux-ghcjs-0.2.1-ghc8_0_2/cards-common-0.0.0/data/json/RIXSets-y.json: openBinaryFile: does not exist (no such file or directory)
    CallStack (from HasCallStack):
      error, called at src/Gen2/TH.hs:338:70 in ghcjs-0.2.1-ArOyRzZQt3EIHrXqYO2Mr1:Gen2.TH

-}


-- b'AllSetsX :: ByteString
-- b'AllSetsX = $(embedDataFile fp'AllSetsX)

-- b'RealSetsX :: ByteString
-- b'RealSetsX = $(embedDataFile fp'RealSetsX)

-- b'RIXSetsArray :: ByteString
-- b'RIXSetsArray = $(embedDataFile fp'RIXSetsArray)

----------------------------------------

-- b'RIXSetsObject :: ByteString
-- b'RIXSetsObject = $(embedDataFile fp'RIXSetsObject)

-- b'RealSetsX :: ByteString
-- b'RealSetsX = $(embed'RealSetsX)

-- b'RIXSetsObject :: ByteString
-- b'RIXSetsObject = $(embed'RIXSetsObject)

{--}

----------------------------------------
