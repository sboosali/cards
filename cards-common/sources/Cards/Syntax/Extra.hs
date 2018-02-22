{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}

{-| utitilies.



-}
module Cards.Syntax.Extra where

import qualified Text.Parsers.Frisby as Frisby
import           Text.Parsers.Frisby hiding (text, (<>))
--import Text.Parsers.Frisby (P,PM)

import Enumerate
import Enumerate.Function

import qualified Data.Text.Lazy as T

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map

import Prelude.Spiros hiding (P)

----------------------------------------

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
text = toS > Frisby.text > fmap toS

texts :: [Text] -> P s Text  
texts = fmap text > choice

alias :: Text -> a -> P s a
alias t x = text t $> x

aliases :: [(Text,a)] -> P s a
aliases = fmap (alias&uncurry) > choice

vocabulary :: [(Text,a)] -> P s a
vocabulary = aliases 

-- aliases :: [(Text,a)] -> P s a
-- aliases = fmap (alias&uncurry) > choice 

quotable :: P s a -> P s a
quotable pUnquoted = p
  where
  pQuoted = quoted pUnquoted
  p       = pUnquoted // pQuoted

  -- pQuoted = {- <- rule$ -} quoted pUnquoted
  -- p       = {- <- rule$ -} pUnquoted // pQuoted

quoted :: P s a -> P s a
quoted = between (char '"') (char '"') 

----------------------------------------