{-# LANGUAGE OverloadedStrings #-}

{-|

-}
module MTGJSON.Main where

import MTGJSON.Extra
import MTGJSON

import Control.Exception

--import Prelude.Spiros

----------------------------------------

{-|

@

--TODO 
> readDataFile AllDataFile >>= (pSetsObject>return) >>= evaluate 
Left "Error in $.UNH.cards[15].cmc: expected Natural, encountered floating number 0.5"


@

-}

main :: IO ()
main = do
  putStrLn ""
  
  bSetsMetadata <- readDataFile SetsDataFile
  --TODO filesystem requires `backend`-only
  let theSets = pSetsMetadata bSetsMetadata
  print theSets
  
  nothing


----------------------------------------

parseSetsFile :: IO (Either String SetsObject)
parseSetsFile = readDataFile AllDataFile >>= (pSetsObject>return) >>= evaluate

