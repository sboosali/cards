{-# LANGUAGE OverloadedStrings #-}

{-|


-}
module Cards.Syntax.MagicCardsInfo.Validator where

--import Cards.Syntax.Extra
import Cards.Syntax.MagicCardsInfo.Types
--import Cards.Syntax.MagicCardsInfo.Parser

import Prelude.Spiros

----------------------------------------

validate :: Syntax_ -> Either SyntaxError Syntax
validate _ = Left (SyntaxError "")

----------------------------------------
