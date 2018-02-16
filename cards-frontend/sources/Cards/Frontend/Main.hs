
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-|

-}
module Cards.Frontend.Main
  ( mainjs
  ) where

import           Prelude (IO)
import           Cards.Frontend.Extra (MonadJSM)

import qualified Cards.Frontend        as Cards 
import qualified Cards.Frontend.Runner as Runner 

mainjs :: (MonadJSM IO) => IO ()
mainjs = Runner.mainWidgetWithFrontend Cards.frontend
