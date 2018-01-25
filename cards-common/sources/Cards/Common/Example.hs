{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference

module Cards.Common.Example where

-- import Cards.Common

import System.Environment

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

mainWith s = do
 putStrLn s
 putStrLn "(Cards.Common.Example...)"

