module Main ( main ) where

import qualified Cards.Frontend.Main as Frontend 

import Language.Javascript.JSaddle.Warp (run)

main = run 3709 Frontend.main

