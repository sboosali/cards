
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference

{-| This module provides an example program. 
Please read the source too <https://hackage.haskell.org/package/cards-backend/docs/src/Cards-Backend-Example.html (direct Hackage link)>. 

Being a @library@ module, it's typechecked with the package, 
and thus should always build.

Only public interfaces are imported (i.e. no @.Internal@s),
and there are minimal other dependencies. 

'main' is executed by the @cards-backend-example@ executable. 

-}
module Cards.Backend.Example where

import Cards.Backend()

import System.Environment

{-|

Running:

@
cabal build && cabal run example-cards-backend
@

@
stack build && stack exec -- example-cards-backend
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
 putStrLn "(Cards.Backend.Example...)"

