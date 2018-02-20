{-# LANGUAGE RankNTypes, RecordWildCards #-}

{-|

-}
module JSaddleWebSocketsReloader
  ( reloadWidgetWith
 --  , runWith
  ) where

import qualified JSaddleWebSocketsRunner as JSaddleWebSockets

import Reflex.Dom.Core (Widget)

import Prelude 
--import Spiros.Prelude (IO, FilePath, Int, Show)

----------------------------------------
  
data DevelopmentConfig = DevelopmentConfig
 { developmentServerPort      :: Int
 , developmentStaticDirectory :: FilePath
 } deriving (Show)
 -- JSaddleRunnerConfig
 
----------------------------------------

{-|

-}
reloadWidgetWith
  :: DevelopmentConfig
  -> (forall x. Widget x ())
  -> IO ()
reloadWidgetWith = runWith

----------------------------------------

runWith 
  :: DevelopmentConfig
  -> (forall x. Widget x ())
  -> IO ()
runWith DevelopmentConfig{..} = JSaddleWebSockets.runWith developmentStaticDirectory developmentServerPort

----------------------------------------
  
{-

JSaddleWarp.run

JSaddleWarp.debug

-}
