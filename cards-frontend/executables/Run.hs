{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (
    runner
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.Foldable (traverse_)

import Reflex.Dom.Core

import           Network.Wai.Handler.Warp               (defaultSettings,
                                                         runSettings, setPort,
                                                         setTimeout)
import           Network.WebSockets                     (defaultConnectionOptions)

import           Language.Javascript.JSaddle.Run        (syncPoint)
import           Language.Javascript.JSaddle.WebSockets
import           Network.Wai.Middleware.Static

import           System.FilePath                        ((</>))
import           System.Directory                       (listDirectory)
import qualified Data.Text                              as Text
import qualified Data.Map                               as Map

runner' ::
  FilePath ->
  Int ->
  (forall x. Widget x ()) ->
  IO ()
runner' cssPath port w =
  do
    cssFiles <- listDirectory $ "." </> cssPath
    let
      f = do
        let
          stylesheet s =
            elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", s)]) $
              return ()
        mainWidgetWithHead
          (traverse_ (\f -> stylesheet . Text.pack $ cssPath </> f) cssFiles)
          w
      serveFiles = staticPolicy $ hasPrefix cssPath

    debugWrapper $ \refreshMiddleware registerContext -> do
      app <- jsaddleOr 
               defaultConnectionOptions 
               (registerContext >> f >> syncPoint) 
               (refreshMiddleware jsaddleApp)
      runSettings (setPort port (setTimeout 3600 defaultSettings)) $
        serveFiles app

runner ::
  (forall x. Widget x ())
  -> IO ()
runner =
  runner' "css" 8080
