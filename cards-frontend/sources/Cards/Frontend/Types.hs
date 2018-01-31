{-# LANGUAGE NoImplicitPrelude #-}

{-| The core types. 

This module mostly defines types 
(i.e. @data@, @newtype@, @type@, @class@, @instance@) 
and whatever values are necessary for instances.

-}
module Cards.Frontend.Types where
--import Cards.Frontend.Extra

--import qualified Data.Text as T
import Data.Text (Text)
--import Data.Text (pack, unpack)

--import Text.Read (readMaybe)
-- import Data.Monoid
-- import System.Environment

----------------------------------------

type Query = Text
type Results = [Result]
type Result = Card  -- Text

type CardDatabase = [Card]
data Card = Card 
 { _cardName :: Text 
 , _cardText :: Text
 }

----------------------------------------