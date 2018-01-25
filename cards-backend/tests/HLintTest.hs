-- (by ekmett via lens)
-- This module runs HLint on the source tree.

module Main where

import Control.Monad
import Language.Haskell.HLint
import System.Environment
import System.Exit

{-|

Usage:

you can hide code from hlint, 
by using cpp in the source:

    #ifndef HLINT
    ...
    #endif

-}
main :: IO ()
main = do
    args <- getArgs
    hints <- hlint $ ["src", "--cpp-define=HLINT", "--cpp-ansi"] ++ args
    unless (null hints) exitFailure
    
