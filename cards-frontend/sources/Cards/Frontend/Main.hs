
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-|

-}
module Cards.Frontend.Main
  ( mainjs
  ) where

--import           Prelude (IO)
import Language.Javascript.JSaddle (JSM)

  -- , JSM(..)
  -- , MonadJSM(..)
  -- , liftJSM

import qualified Cards.Frontend        as Cards 
import qualified Cards.Frontend.Runner as Runner 

mainjs :: JSM ()  
mainjs = do
--  Cards.initialize
  Runner.mainWidgetWithFrontend Cards.frontend

-- mainjs :: (MonadJSM m) => m ()  
-- {-# SPECIALIZE mainjs :: JSM () #-}
