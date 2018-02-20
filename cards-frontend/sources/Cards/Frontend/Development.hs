{-# LANGUAGE RankNTypes #-}

{-|

e.g.

@
> :l Cards.Frontend.Development
> reload
@

or

@
> import qualified Cards.Frontend.Development as Development
> :r Cards.Frontend.GUI
> Development.reloadFrontend frontend
@

-}
module Cards.Frontend.Development
  ( reload
  ) where

import qualified Cards.Frontend.GUI as GUI

import           JSaddleWebSocketsReloader (DevelopmentConfig(..))
import qualified JSaddleWebSocketsReloader as JSaddleWebSockets
x
----------------------------------------

{-| hotswap the frontend ('GUI.frontend'). 

-}
reload :: IO ()  
reload = reloadFrontend GUI.frontend

{-| hotswap a (parametrized) @<body>@ 'Widget'. 

-}
reloadWidget :: Frontend -> IO ()
reloadWidget (Frontend (SomeWidget _) wBodyWithRunner)
  = case wBodyWithRunner __JAVASCRIPT_RUNNER__ of
      SomeWidget wBody -> do
        JSaddleWebSockets.reloadWidgetWith config $ do
          wBody

----------------------------------------
-- Constants

config :: DevelopmentConfig
config = DevelopmentConfig{..}
  where
  developmentServerPort      = 3911
  developmentStaticDirectory = "static/css"

-- -- |
-- directoryStatic :: FilePath
-- directoryStatic = "static/css"
-- -- _STATIC_DIRECTORY

-- -- |
-- portJSaddleWebSockets :: Int
-- portJSaddleWebSockets = 3911

-- | the current executable's runner.
-- like @System.os@, this is constant within the program's execution.
__JAVASCRIPT_RUNNER__ :: JAVASCRIPT_RUNNER
__JAVASCRIPT_RUNNER__ = JSADDLEWEBSOCKETS -- JSADDLEWARP6

----------------------------------------
  
{-

-- reloadWidget
--   :: (JAVASCRIPT_RUNNER -> (forall x. Widget x ()))
--   -> IO ()
-- reloadWidget wBodyWith = do
--   runWith wBody
--   where
--   wBody = wBodyWith __JAVASCRIPT_RUNNER__
  

> :l Cards.Frontend.GUI
> :l Cards.Frontend.Development
> reloadFrontend frontend

-}


  