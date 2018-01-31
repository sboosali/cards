{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-| The core definitions. 

-}
module Cards.Frontend.Core where

import Cards.Frontend.Extra
import Cards.Frontend.Types

import Reflex hiding (Query)
--import qualified Reflex as R
import Reflex.Dom hiding (Query)

--import qualified Control.Lens as L

--import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Text as T
import Data.Text (Text)
--import Data.Text (pack, unpack)

--import Text.Read (readMaybe)
-- import Data.Monoid
-- import System.Environment

----------------------------------------

{-|

Naming:
* "w" for widget.

-}
wHead :: MonadWidget t m => m ()
wHead = do
  el "title" $ text "MTG Card Search"
  styleSheet "css/style.css"

  where

  styleSheet url = elAttr "link" (styleAttributes url) blank

  styleAttributes url =
   [ "rel" -: "stylesheet"
   , "type"-: "text/css"
   , "href"-: url
   ] 

----------------------------------------

{-|

Naming:
* "w" for widget.

-}
wBody :: MonadWidget t m => m ()
wBody = do

  oSearch <- textInput iSearch
  let dSearch = value oSearch
  let dQuery = dSearch -- & fmapMaybe nonEmptyString
 
  let dResults = dQuery <&> execQuery defaultCardDatabase

  let dTable = dResults <&> formatResults
  _wTable <- dyn dTable 

  blank

  --where
  --nonEmptyString = fromPredicate T.null

{- | for `fmapMaybe`

-}
fromPredicate :: (a -> Bool) -> (a -> Maybe a)
fromPredicate p = \x -> if p x then Just x else Nothing

----------------------------------------

initialQuery :: Text
initialQuery = "fire"
 -- initialQuery = "n:fire t:kavu"

placeholderQuery :: Text
placeholderQuery = "fire"

queryAttributes :: Reflex t => Dynamic t (Map Text Text)
queryAttributes = constDyn as
 where
 as = mconcat
     [ "placeholder" =: placeholderQuery  -- /= initialQuery
     ]

 -- as = mempty
 -- as = 
 --     [ "placeholder" -: initialQuery
 --     ]

iSearch :: Reflex t => TextInputConfig t
iSearch = def 
   { _textInputConfig_inputType      = "search"           -- "text"
   , _textInputConfig_attributes     = queryAttributes
  -- , _textInputConfig_initialValue = initialQuery
   }
  
----------------------------------------

execQuery :: CardDatabase -> Query -> Results
execQuery db = parseQuery >>> runQuery db 

-- execQuery :: CardDatabase -> Query -> Text
-- execQuery db = parseQuery >>> runQuery db >>> formatResults 

parseQuery :: Text -> Query 
parseQuery q = T.toLower q

runQuery :: CardDatabase -> Query -> Results
runQuery db q = db & filter (\Card{..} -> q `T.isInfixOf` _cardName) 

 -- (_cardName == q) 
 -- & fmap _cardText

----------------------------------------

formatResults :: (MonadWidget t m) => Results -> m ()
formatResults
 = traverse_ formatCard

 -- = fmap formatCard
 -- > intersperse (sequence_ [div_, text "========================================", div_])
 -- > sequence_

formatCard :: (MonadWidget t m) => Card -> m ()
formatCard Card{..} = divClass "card" $ do
 div $ text _cardName
 div $ text _cardText

-- formatResults :: Results -> Text
-- formatResults
--  = fmap (\Card{..} -> _cardName <> "\n" <> _cardText)
--  > T.intercalate "\n\n========================================\n\n"

----------------------------------------

defaultCardDatabase :: CardDatabase
defaultCardDatabase = fmap (Card & uncurry)
 [ "fire" -: "deal 3 damage", "ice" -: "tap target permanent", "kavu firemaw" -: "FIREMAW" ]

----------------------------------------

 -- def = TextInputConfig { _textInputConfig_inputType = "text"
 --                        , _textInputConfig_initialValue = ""
 --                        , _textInputConfig_setValue = never
 --                        , _textInputConfig_attributes = constDyn mempty }


{-

(<&?>) ::
(<&?>) = flip fmapMaybe
(<&?>) xs p = fmapMaybe p xs


<head>
 <title>MTG Card Search</title>

<meta charset="utf-8">

<meta name="description" content="MTG Card Search. Write smart queries (e.g. 'all creatures their creature types in their text', like lords), and get card results fast. Works offline, and provides some accessibility features like a text-only mode. Currently, the only card game that's indexed is Magic The Gathering.">
<meta name="author" content="Spiros Boosalis">

<meta name="viewport" content="width=device-width, initial-scale=1.0">

<meta property="og:title" content="MTG Card Search">
<meta property="og:image" content="X.png">
<meta property="og:description" content="...">

</head>

-}

----------------------------------------