module Main ( main ) where

import qualified Cards.Frontend.Main as Frontend

import Language.Javascript.JSaddle (runJSM)

main = runJSM Frontend.mainjs
