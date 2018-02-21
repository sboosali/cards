
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference

module Cards.Common.Example where

-- import Cards.Common
import Cards.Syntax.MagicCardsInfo.Parser 

import System.Environment

import Prelude

----------------------------------------

{-|

@
stack build && stack exec -- example-cards-common
@
-}
main :: IO ()
main = do
 arguments <- getArgs >>= \case
  [s] -> return (s)
  _ -> return ("")
 mainWith arguments

mainWith _ = do
 putStrLn ""
 putStrLn "(Cards.Common.Example...)"


----------------------------------------

{-
 good
 bad

 putStrLn ""
 putStrLn ""
 
good = do
 let s = "(1+2*3+4)*2"
 let x = (1+2*3+4)*2
 putStrLn""
 print s
 print x
 let y = runAdditive s
 let z = runAdditive' s
 print y
 print $ y == x
 print z
 print $ z == (Just x,"")

bad = do
 let s = "1+2)"
 putStrLn""
 print s
 let y = runAdditive s
 let z = runAdditive' s
 print y
 print z
-}

{-

(1+2*3+4)*2
22
22
True
(Just 22,"")
True

1+2)
3
(Nothing,"1+2)")

-}
