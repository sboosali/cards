{-# LANGUAGE TemplateHaskell #-}
{-TODO-}

{-|

transitively imports @Paths_cards_common@. 

-}
module MTGJSON.Macros where

-- import MTGJSON.AllSets
-- import MTGJSON.Paths

-- import Data.ByteString (ByteString)

-- import Prelude.Spiros hiding (ByteString)
-- import Prelude (error)

----------------------------------------

-- embeddedRealSetsY :: ByteString
-- embeddedRealSetsY = $(embedDataFile CardsDataFile)

-- parsedRealSetsY :: Either String SetsObject
-- parsedRealSetsY = pSetsObject (toS embeddedRealSetsY)

-- validatedRealSetsY :: SetsObject
-- validatedRealSetsY = parsedRealSetsY
--   & either (error) id
--   --TODO macro

{-LOL


1.

[18 of 20] Compiling MTGJSON.Macros   ( sources/MTGJSON/Macros.hs, /home/sboo/haskell/cards/dist-newstyle/build/x86_64-linux/ghc-8.0.2/cards-common-0.0.0/build/MTGJSON/Macros.o )

stack overflow: use +RTS -K<size> to increase it


2.

The build process was
killed (i.e. SIGKILL). The typical reason for this is that there is not enough
memory available (e.g. the OS killed a process using lots of memory).



-}

----------------------------------------
