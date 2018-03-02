{-# LANGUAGE OverloadedStrings #-}

{-|

-}
module MTGJSON.Constants where

import qualified Data.Text.Lazy as T

import Prelude.Spiros

----------------------------------------

{- | @Unicode Character <Private Use, First> (U+E000)@

@
> generalCategory '\xE000'
PrivateUse
@

@
In Unicode, a Private Use Area (PUA) is a range of code points that, by definition, will not be assigned characters by the Unicode Consortium.[1] Currently, three private use areas are defined: one in the Basic Multilingual Plane (U+E000–U+F8FF), and one each in, and nearly covering, planes 15 and 16 (U+F0000–U+FFFFD, U+100000–U+10FFFD). The code points in these areas cannot be considered as standardized characters in Unicode itself. They are intentionally left undefined so that third parties may define their own characters without conflicting with Unicode Consortium assignments. Under the Unicode Stability Policy,[2] the Private Use Areas will remain allocated for that purpose in all future Unicode versions.
@

-}
cPseudoNewline :: Char
cPseudoNewline = '\xE000'

tPseudoNewline :: Text
tPseudoNewline = T.pack [cPseudoNewline]

  -- '\xE000'
  -- '\0xEE\0x80\0x80'
  -- '\uE000'

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
