
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-| 

-}
module Cards.Syntax.MagicCardsInfo.Compiler where

--import           Cards.Syntax.MagicCardsInfo.Types hiding (Query)
import qualified Cards.Syntax.MagicCardsInfo.Types as MagicCardsInfo
--import Cards.Syntax.MagicCardsInfo.Validator 

import Cards.Query.Types

----------------------------------------

compile :: MagicCardsInfo.Query i j -> Query
compile _ = Query

----------------------------------------
