{-# LANGUAGE NoImplicitPrelude, PackageImports #-}

module Main ( main ) where

import Prelude                (IO)
import Control.Monad.IO.Class (liftIO)

import qualified "cards-frontend" Cards.Frontend.Main as Frontend

import qualified "jsaddle-webkit2gtk" Language.Javascript.JSaddle.WebKitGTK as GTK

main :: IO ()
main = GTK.run mainjs
  where
  mainjs = liftIO Frontend.main --TODO lol
  -- mainWithRunner

{-NOTE

GTK.run :: JSM () -> IO ()
GTK.run mainjs = do
    _ <- Gtk.init Nothing
   ...

-}
