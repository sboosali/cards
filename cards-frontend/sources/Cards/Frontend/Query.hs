
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-| The query language(s) and search functionality. 

-}
module Cards.Frontend.Query where

import Cards.Frontend.Extra
import Cards.Frontend.Types

import qualified Data.Text as T
  
----------------------------------------

-- {-|

-- -}
-- data LogicSyntax

----------------------------------------

-- {-|

-- a @Prolog@-like language (simpler than / a subset thereof), with unification

-- -}
-- data LogicQuery

----------------------------------------


----------------------------------------

{- | non-empty and 'normalize'd (case insensitive, etc).

-}
validateQuery :: RawQuery -> Maybe ValidQuery
validateQuery
  = normalize
  > fromPredicate (T.null > not)
  > fmap ValidQuery

----------------------------------------

-- parseQuery :: Text -> RawQuery 
-- parseQuery 
--   = normalize 
-- --  > RawQuery

 -- (_cardName == q) 
 -- & fmap _cardText
----------------------------------------

abbreviations :: Map Text Text
abbreviations =
  [ "cmc"-: "converted mana cost"
  , "kill"-: "destroy"
  ]

----------------------------------------

normalizeS :: String -> Text
normalizeS = T.pack > normalize

normalize :: Text -> Text
normalize
  = ignoreCasing
  > stripRedundantWhitespace
  where

  ignoreCasing
    = T.toLower

  stripRedundantWhitespace
    = T.strip

  --TODO preserve in body
  -- stripRedundantWhitespace
  --   = T.split isSpace
  --   > mconcat

 -- stripWhitespace = T.strip
  
----------------------------------------
