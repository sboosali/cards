{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}

{-# LANGUAGE ScopedTypeVariables #-}

{-| utitilies.



-}
module Cards.Syntax.Extra where

import qualified Text.Parsers.Frisby      as F
import qualified Text.Parsers.Frisby.Char as F
import           Text.Parsers.Frisby hiding (text, (<>))
--import Text.Parsers.Frisby (P,PM)

import Enumerate
import Enumerate.Function

import qualified Data.Text.Lazy as T

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import Prelude.Spiros hiding (P)

----------------------------------------

{-|

group a pair of related parsers:

* one ('_pQuoted') parses a value within quotations,
* the other  ('_pUnquoted') parses that same value outside quotations and thus "more conservatively". 

-}
data QuotableParser s a = QuotableParser
 { _pUnquoted :: P s a
 , _pQuoted   :: P s a
 } deriving (Functor)

-- | pairwise
instance Applicative (QuotableParser s) where
  pure x = QuotableParser (pure x) (pure x)
  (QuotableParser p q) <*> (QuotableParser p' q') =
      QuotableParser (p <*> p') (q <*> q')

-- | pairwise
instance Alternative (QuotableParser s) where
  empty = QuotableParser empty empty
  (QuotableParser p q) <|> (QuotableParser p' q') =
      QuotableParser (p <|> p') (q <|> q')

-- | same as 'Alternative'
instance Semigroup (QuotableParser s a) where
  (<>) = (<|>)

-- | same as 'Alternative'
instance Monoid (QuotableParser s a) where  
  mempty = empty
  mappend = (<>)

singletonQuotableParser :: P s a -> QuotableParser s a
singletonQuotableParser p = QuotableParser p p

-- | 
quotable' :: P s a -> P s a 
quotable' = singletonQuotableParser > quotable

-- | 
quotable :: QuotableParser s a -> P s a
quotable QuotableParser{..} = p // q
  where
  p = quoted _pUnquoted
  q =        _pQuoted

----------------------------------------
-- `frisby`

type G s a = PM s (P s a)  
--type Grammar s a = PM s (P s a)
--type Parser  s a = P s a

type Complete a = (Maybe a, String)

rule :: P s a -> G s a
rule = newRule

complete :: PM s (P s a) -> PM s (P s (Complete a))
complete grammar = mdo
  p <- grammar
  let q = (,) <$> (fmap Just (p <<- eof) // unit Nothing) <*> rest
  return q

text :: Text -> P s Text
text = toS > F.text > fmap toS

texts :: [Text] -> P s Text  
texts = fmap text > choice

alias :: Text -> a -> P s a
alias t x = text t $> x

aliases :: [(Text,a)] -> P s a
aliases = fmap (alias&uncurry) > choice

vocabulary :: [(Text,a)] -> P s a
vocabulary = aliases

integer :: (Num i) => P s i
integer = p <&> (readMay > maybe 0 fromInteger) --TODO explicit failure?
  where
  p = many1 F.digit

-- | makes a PEG parser from a @Printer@ for a finite type. 
printer
  :: forall a s.
     (Enumerable a) -- , Ord a)
  => (a -> Text)
  -> P s a
printer f = aliases g
  where
  g :: [(Text,a)]
  g = reifyFunction f & fmap swap 

  swap (x,y) = (y,x)

-- printer f = choice ps
--   where
--   ps :: [P s a]
--   ps = g <&> 

-- aliases :: [(Text,a)] -> P s a
-- aliases = fmap (alias&uncurry) > choice 

-- -- | 
-- quotable :: P s a -> P s a -> P s a
-- quotable pUnquoted pQuoted = p // q
--   where
--   p = quoted pUnquoted
--   q =        pQuoted

-- quotable :: P s a -> P s a
-- quotable pUnquoted = p
--   where
--   pQuoted = quoted pUnquoted
--   p       = pUnquoted // pQuoted

  -- pQuoted = {- <- rule$ -} quoted pUnquoted
  -- p       = {- <- rule$ -} pUnquoted // pQuoted

quoted :: P s a -> P s a
quoted = between (char '"') (char '"')
  
----------------------------------------
-- `text`

braces :: Text -> Text
braces t = "{" <> t <> "}"

char2text :: Char -> Text
char2text = T.singleton 

show' :: (Show a, StringConv String s) => a -> s
show' = show > toS

----------------------------------------
-- `enumerate-function`

invert' :: (Enumerable a, Ord a, Ord b) => (a -> b) -> (Map b a)
invert' = fromFunction > invertMap > Map.map NonEmpty.head
  
-- invert' :: (Enumerable a, Ord a, Ord b) => (a -> b) -> (Map b a)
-- invert' = fromFunction > invertMap > Map.update 
-- update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a

----------------------------------------

  