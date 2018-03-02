{-# LANGUAGE OverloadedStrings #-}

{-|

-}
module MTGJSON.Main where

import MTGJSON.Extra
import MTGJSON

--import Prelude.Spiros

----------------------------------------

main :: IO ()
main = do
  putStrLn ""
  
  bSetsMetadata <- readDataFile SetsDataFile
  --TODO filesystem requires `backend`-only
  let theSets = pSetsMetadata bSetsMetadata
  print theSets

  nothing

----------------------------------------

