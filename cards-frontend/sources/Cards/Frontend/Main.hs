{-| simple utilities to make defining the executables easier, without depending on non-portable (platform-specific) dependencies. 

TODO use module signatures, not function arguments? 

-}
module Cards.Frontend.Main
  ( main
  -- , mainWithDefaultRunner
  -- , defaultRunner
  , mainWithRunner
  ) where

import           Prelude.Spiros (IO, liftIO, ($))

import qualified Cards.Frontend        as Cards
import           Cards.Frontend.Types
 ( Runner
 , Frontend(..)
 , JAVASCRIPT_RUNNER(BROWSER)
 , SomeWidget(..)
 --, XWidget_
 )

import qualified Reflex.Dom (mainWidgetWithHead)

----------------------------------------

-- | @= 'mainWithDefaultRunner'@
main :: IO ()
main = mainWithDefaultRunner

-- | @= 'mainWithRunner' @
mainWithDefaultRunner :: IO () 
mainWithDefaultRunner = mainWithRunner defaultRunner

-- | @~ "Reflex.Dom.mainWidgetWithHead"@
defaultRunner :: Runner
defaultRunner (Frontend (SomeWidget wHead) wBodyWithRunner)
  = liftIO $ do
      Reflex.Dom.mainWidgetWithHead
        wHead
        wBody
  where
  --wBody :: XWidget_
  SomeWidget wBody = wBodyWithRunner __JAVASCRIPT_RUNNER__
  
  -- the current executable's runner.
  -- like @System.os@, this is constant within the program's execution.
  __JAVASCRIPT_RUNNER__ :: JAVASCRIPT_RUNNER
  __JAVASCRIPT_RUNNER__ = BROWSER

-- | calls our frontend (@"Cards.frontend"@) with the given 'Runner'. 
mainWithRunner :: Runner -> IO () -- Frontend -> IO () -- JSM ()  
mainWithRunner mainWidgetWithFrontend = do
  Cards.initialize  
  mainWidgetWithFrontend Cards.frontend

----------------------------------------

-- mainjs :: (MonadJSM m) => m ()  
-- {-# SPECIALIZE mainjs :: JSM () #-}

{-

mainWidgetWithFrontend :: Runner
mainWidgetWithFrontend (Frontend (SomeWidget wHead) wBodyWithRunner)
  = case wBodyWithRunner BROWSER of
      SomeWidget wBody -> liftIO $ do
          Reflex.Dom.mainWidgetWithHead
            wHead
            wBody
-}
