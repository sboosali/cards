{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

-}
module MTGJSON.AllSets.Enums.Mana where

import MTGJSON.Extra

import Control.Lens (makePrisms)

----------------------------------------

newtype ManaCost = ManaCost Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''ManaCost

----------------------------------------

newtype ManaSymbol = ManaSymbol Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''ManaSymbol

----------------------------------------

--TODO  
toManaCost :: Maybe Text -> ManaCost
toManaCost = maybe noManaCost ManaCost

noManaCost :: ManaCost
noManaCost = ""

----------------------------------------

whiteSymbol :: ManaSymbol
whiteSymbol = "W"

blueSymbol :: ManaSymbol
blueSymbol = "U"

blackSymbol :: ManaSymbol
blackSymbol = "B"

redSymbol :: ManaSymbol
redSymbol = "R"

greenSymbol :: ManaSymbol
greenSymbol = "G"

----------------------------------------

colorlessSymbol :: ManaSymbol
colorlessSymbol = "C"

snowSymbol :: ManaSymbol
snowSymbol = "S"

energySymbol :: ManaSymbol
energySymbol = "E"

variableSymbol :: ManaSymbol
variableSymbol = "X"

----------------------------------------

phyrexian :: ManaSymbol -> ManaSymbol
phyrexian (ManaSymbol s) = ManaSymbol ("{P" <> s <> "}")

phyrexianWhite :: ManaSymbol
phyrexianWhite = phyrexian whiteSymbol

phyrexianBlue :: ManaSymbol
phyrexianBlue = phyrexian blueSymbol

phyrexianBlack :: ManaSymbol
phyrexianBlack = phyrexian blackSymbol

phyrexianRed :: ManaSymbol
phyrexianRed = phyrexian redSymbol

phyrexianGreen :: ManaSymbol
phyrexianGreen = phyrexian greenSymbol

----------------------------------------

monohybrid :: ManaSymbol -> ManaSymbol
monohybrid (ManaSymbol s) = ManaSymbol ("{2/" <> s <> "}")

monohybridWhite :: ManaSymbol
monohybridWhite = monohybrid whiteSymbol

monohybridBlue :: ManaSymbol
monohybridBlue = monohybrid blueSymbol

monohybridBlack :: ManaSymbol
monohybridBlack = monohybrid blackSymbol

monohybridRed :: ManaSymbol
monohybridRed = monohybrid redSymbol

monohybridGreen :: ManaSymbol
monohybridGreen = monohybrid greenSymbol

----------------------------------------

genericSymbol :: Natural -> ManaSymbol
genericSymbol n = ManaSymbol $ show' n

zeroSymbol :: ManaSymbol
zeroSymbol = genericSymbol 0

oneSymbol :: ManaSymbol
oneSymbol = genericSymbol 1

twoSymbol :: ManaSymbol
twoSymbol = genericSymbol 2

threeSymbol :: ManaSymbol
threeSymbol = genericSymbol 3

fourSymbol :: ManaSymbol
fourSymbol = genericSymbol 4

fiveSymbol :: ManaSymbol
fiveSymbol = genericSymbol 5

sixSymbol :: ManaSymbol
sixSymbol = genericSymbol 6

sevenSymbol :: ManaSymbol
sevenSymbol = genericSymbol 7

eightSymbol :: ManaSymbol
eightSymbol = genericSymbol 8

nineSymbol :: ManaSymbol
nineSymbol = genericSymbol 9

tenSymbol :: ManaSymbol
tenSymbol = genericSymbol 10

elevenSymbol :: ManaSymbol
elevenSymbol = genericSymbol 11

twelveSymbol :: ManaSymbol
twelveSymbol = genericSymbol 12

thirteenSymbol :: ManaSymbol
thirteenSymbol = genericSymbol 13

fourteenSymbol :: ManaSymbol
fourteenSymbol = genericSymbol 14

fifteenSymbol :: ManaSymbol
fifteenSymbol = genericSymbol 15

sixteenSymbol :: ManaSymbol
sixteenSymbol = genericSymbol 16

seventeenSymbol :: ManaSymbol
seventeenSymbol = genericSymbol 17

eighteenSymbol :: ManaSymbol
eighteenSymbol = genericSymbol 18

nineteenSymbol :: ManaSymbol
nineteenSymbol = genericSymbol 19

twentySymbol :: ManaSymbol
twentySymbol = genericSymbol 20

----------------------------------------

{-

 ManaSymbol Char

-}
