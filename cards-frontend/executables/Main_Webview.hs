module Main ( main ) where

import qualified Cards.Frontend.Main as Frontend 

import Language.Javascript.JSaddle.WKWebView (run)

main = run Frontend.main
