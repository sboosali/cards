{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-|

-}
module Cards.Frontend.Extra
 ( module Cards.Frontend.Extra
 , module Prelude.Spiros
 ) where

import Reflex.Dom

import Prelude.Spiros hiding (Text,div)

----------------------------------------

-- | @<div>...</div>@
div :: (MonadWidget t m) => m () -> m ()
div = el "div"

-- | the HTML equivalent of the newline @"\n"@. 
div_ :: (MonadWidget t m) => m ()
div_ = div blank

