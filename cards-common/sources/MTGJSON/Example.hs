{-# LANGUAGE OverloadedStrings #-}

{-|

-}
module MTGJSON.Example where 

import MTGJSON.Extra -- (nothing, toSL, toS)

import MTGJSON.AllSets -- .Schema 
import MTGJSON.Paths
-- import qualified Cards.MTGJSON.Macros as Ms
-- import qualified Cards.MTGJSON.ByteStringLiterals as Bs

--import qualified Data.ByteString.Lazy.Char8 as B8

--import qualified Data.Text    as T
import qualified Data.Text.IO as T

import System.IO.Error
--import Control.Exception
--import qualified Control.Exception as E

----------------------------------------

main :: IO ()
main = do
  putStrLn ""

  -- putStrLn ""
  -- putStrLn "[embedDataFile]"
  -- putStrLn ""
  -- do
  --     let b = toSL Ms.b'RIXSetsY  
  --     let c' = pSetsObject b
  --     print c'

  -- putStrLn ""
  -- putStrLn "[literals]"
  -- putStrLn ""
  -- do
  --     let b = toSL Bs.b'RIXSetsY  
  --     let c' = pSetsObject b
  --     print c'

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

  -- e' <- tryIOError $ do
  --     b <- readDataFile ("cards-common/" ++ fp'RIXSetsY)
  --     --TODO filesystem requires `backend`-only
  --     let c' = pSetsObject b

  --     putStrLn "[json]"
  --     print c'

  --     putStrLn "[bytes]"
  --     T.putStrLn $ toS b

  --     -- putStrLn "[ushow]"
  --     -- let s = toS $ ushow b
  --     -- 
  --     -- T.writeFile "cards-common/includes/RIXSets-y.txt" s
  --      --NOTE

  -- print e'

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
