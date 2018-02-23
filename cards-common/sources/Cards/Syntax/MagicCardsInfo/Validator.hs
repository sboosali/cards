{-# LANGUAGE OverloadedStrings #-}

{-|


-}
module Cards.Syntax.MagicCardsInfo.Validator where

--import Cards.Syntax.Extra
import Cards.Syntax.MagicCardsInfo.Types
--import Cards.Syntax.MagicCardsInfo.Parser

import Prelude.Spiros

----------------------------------------

validate :: Query_ i j -> Either SyntaxError (Query i j)
validate _ = Left (SyntaxError "")

----------------------------------------
