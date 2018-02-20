{-# LANGUAGE RankNTypes #-}

{-|

> :l Cards.Frontend.Development
> reload

-}
module Cards.Frontend.Development
  ( reload
  ) where

import qualified Cards.Frontend.GUI as GUI

-- import JSaddleWebSocketsReloader (reloadFrontend)
import qualified JSaddleWebSocketsReloader as Server

----------------------------------------

reload :: IO ()  
reload = Server.reloadFrontend GUI.frontend

----------------------------------------

{-

> :l Cards.Frontend.GUI
> :l Cards.Frontend.Development
> reloadFrontend frontend

-}
