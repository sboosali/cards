module Main ( main ) where

import qualified Cards.Frontend.Main as Frontend

import Language.Javascript.JSaddle.WebKitGTK (run)

main = run Frontend.main
