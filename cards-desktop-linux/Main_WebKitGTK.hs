{-# LANGUAGE NoImplicitPrelude, PackageImports #-}

module Main ( main ) where

import Prelude (IO)

import qualified "cards-frontend" Cards.Frontend.Main as Frontend

import qualified "jsaddle-webkit2gtk" Language.Javascript.JSaddle.WebKitGTK as GTK

main :: IO ()
main = GTK.run Frontend.mainjs

{-NOTE

GTK.run :: JSM () -> IO ()
GTK.run mainjs = do
    _ <- Gtk.init Nothing
   ...

-}
