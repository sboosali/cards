{-# LANGUAGE OverloadedStrings #-}

{-|

-}
module Cards.MTGJSON.Example where 

import Cards.Common.Extra (nothing, toSL)

import Cards.MTGJSON.Schema 
import Cards.MTGJSON.Paths
import Cards.MTGJSON.Macros

import qualified Data.ByteString.Lazy.Char8 as B8 

import System.IO.Error
--import Control.Exception
--import qualified Control.Exception as E
  
----------------------------------------

main :: IO ()
main = do
  putStrLn ""

  putStrLn ""
  putStrLn "[embedDataFile]"
  putStrLn ""
  do
      let b = toSL b'RIXSetsY  
      let c' = pSetsObject b
      print c'

  -- do
  --     let b = toSL b'RealSets
  --     let c' = pSetsObject b
  --     print c'

  -- do
  --     let b = toSL b'RIXSetsArray  
  --     let c' = pSetsArray b
  --     print c'

  putStrLn ""
  putStrLn "[readDataFile]"
  putStrLn ""
  e' <- tryIOError $ do
      b <- readDataFile ("cards-common/" ++ fp'RIXSetsY)
      --TODO filesystem requires `backend`-only
      B8.putStrLn b
      let c' = pSetsObject b
      print c'
  print e'

  {-

  readDataFile fp'RIXSetsY

    Left data/json/RIXSets-y.json: openBinaryFile: does not exist (No such file or directory)
  -}


  putStrLn ""
  nothing

----------------------------------------
  
-- catchIO :: IO a -> (IOError -> IO a) -> IO a
-- catchIO = catch

----------------------------------------
