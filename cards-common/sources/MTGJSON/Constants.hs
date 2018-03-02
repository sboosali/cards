{-# LANGUAGE OverloadedStrings #-}

{-|

-}
module MTGJSON.Constants where

import Prelude.Spiros

----------------------------------------

theOracleSymbolWrappingCharacters :: (Char,Char)
theOracleSymbolWrappingCharacters = ('{', '}')

theOracleSymbolWrappers :: [Char]--TODO rofl
theOracleSymbolWrappers = [x,y]
  where
  (x,y) = theOracleSymbolWrappingCharacters

theOracleWordSeparators :: [Char]
theOracleWordSeparators = " "
  
theOracleLineSeparators :: [Char]
theOracleLineSeparators = "\n"

theOracleReservedTokens :: [String]
theOracleReservedTokens
   = theOracleReservedKeywords
  ++ (theOracleReservedCharacters & fmap (:[]))

theOracleReservedKeywords :: [String]
theOracleReservedKeywords = --TODO
  [ "CARDNAME"
  ]

theOracleReservedCharacters :: [Char]
theOracleReservedCharacters =
  "~{}"
 --  "~{} \n"

----------------------------------------
