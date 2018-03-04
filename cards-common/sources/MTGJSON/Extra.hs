
{-|

-}
module MTGJSON.Extra
 ( module Prelude.Spiros
 , module X
 , module MTGJSON.Extra
 ) where

import qualified MTGJSON.Constants as Constants

----------------------------------------

-- re-exports
import              Enumerate          as X (Enumerable)
import              Data.List.NonEmpty as X (NonEmpty(..))
import              Data.Aeson         as X (eitherDecode)
import "validation" Data.Validation    as X (Validation(..))
import              Data.Monoid        as X (First(..))
import "scientific" Data.Scientific    as X (Scientific)

----------------------------------------

import qualified Data.Aeson        as J 
--import qualified Data.Aeson.Types  as J

import qualified "parsers"  Text.Parser.Combinators as P
-- import qualified "parsers"  Text.Parser.Token    as P
import qualified "parsers"  Text.Parser.Char     as P
import qualified "trifecta" Text.Trifecta as P

import Control.Lens (Wrapped(..))

import Enumerate.Function

import "validation" Data.Validation
--NOTE "Validation" is deprecated in favor of "validation"

import qualified Data.Text.Lazy as T

import Data.ByteString.Lazy (ByteString) 

import qualified Data.Map as Map

----------------------------------------
-- base
import Control.Monad.Fail (MonadFail)
import Data.Coerce
--import Data.Monoid (First(..))

import Prelude.Spiros

----------------------------------------

print2parse
  :: ( Enumerable a
     , Ord        a
     )
  => Print a 
  -> Parse a
print2parse f = f'
  where
  f' = invertInjection f

printer
  :: ( Enumerable a
     , Ord        a
     , P.CharParsing p
     )
  => Print a 
  -> p a
printer f = strings xs
  where
  xs = Map.toList f' --TODO just reifyFunction?
  f' = fromInjective f

----------------------------------------

type Print a = SPrint a
type Parse a = SParse a

type SPrint a = a      -> String
type SParse a = String -> Maybe a 

type TPrint a = a    -> Text 
type TParse a = Text -> Maybe a

----------------------------------------

type V = Validation

success :: a -> Validation e a
success = Success

failure :: e -> Validation (NonEmpty e) a
failure = (:|[]) > Failure

maybe2validation :: e -> Maybe a -> Validation (NonEmpty e) a
maybe2validation e = maybe (failure e) success

----------------------------------------

type List = []

----------------------------------------

type Association k v = [(k,v)]

-- | 'P.choice' of 'P.string's
strings
  :: (P.CharParsing p)
  => Association String a
  -> p a
strings
  = fmap (\(s, a) -> P.string s $> a) 
  > P.choice
  
  -- P.oneOf

-- | 'P.choice' of 'P.chars's
chars
  :: (P.CharParsing p)
  => Association Char a
  -> p a
chars
  = fmap (char2string *** id)
  > strings
  where
  char2string = (:[])

token
  :: (P.CharParsing p)
  => p a -> p a
token p = p <* P.skipMany (P.satisfy $ isWhitespaceExceptNewline)
  where
  isWhitespaceExceptNewline = \c
    -> c /= '\n'
    && isSpace c

-- | 'P.choice' of 'P.symbol's
symbols
  :: (P.TokenParsing p)
  => Association String a
  -> p a
symbols
  = fmap (\(s, a) -> P.symbol s $> a) 
  > P.choice

-- | 'P.choice' of 'P.symbolic's
symbolics
  :: (P.TokenParsing p)
  => Association Char a
  -> p a
symbolics
  = fmap (\(c, a) -> P.symbolic c $> a) 
  > P.choice

-- | 'P.choice' of 'P.symbol's
symbols_
  :: (P.TokenParsing p)
  => [String]
  -> p ()
symbols_
  = fmap (-: ())
  > symbols

-- | 'P.choice' of 'P.symbol's
symbolics_
  :: (P.TokenParsing p)
  => [Char]
  -> p ()
symbolics_
  = fmap (-: ())
  > symbolics

betweenChars
  :: (P.CharParsing p)
  => Char -> Char -> p a -> p a
betweenChars x y = P.between (P.char x) (P.char y)

betweenStrings
  :: (P.CharParsing p)
  => String -> String -> p a -> p a
betweenStrings x y = P.between (P.string x) (P.string y)

-- | Token parser @lineSep p@ parses /one/ or more occurrences of @p@
-- separated by 'newline'. Returns a list of values returned by @p@.
-- 
lineSep :: P.TokenParsing m => m a -> m [a]
lineSep p = P.sepBy1 p P.newline
{-# INLINE lineSep #-}

pseudoLineSep :: P.TokenParsing m => m a -> m [a]
pseudoLineSep p = p `P.sepBy1` pPseudoNewline

pPseudoNewline :: P.TokenParsing m => m ()
pPseudoNewline = () <$
  token (P.char Constants.cPseudoNewline P.<?> "\n")

-- -- | Parses a newline character, @\n@.
-- newline :: TokenParsing m => m ()
-- newline = char '\n' $> ()

result2maybe :: P.Result a -> Maybe a
result2maybe = \case
    P.Success a  -> Just a
    P.Failure _e -> Nothing

endOfLine :: P.CharParsing m => m ()
endOfLine
    = (P.string "\r\n" *> pure ())
  <|> (P.char   '\n'   *> pure ())

----------------------------------------

decoded :: (MonadFail m, J.FromJSON a) => ByteString -> m a
decoded = J.eitherDecode > either fail return 

----------------------------------------

unsafeNatural :: Integral i => i -> Natural
unsafeNatural = fromIntegral

n2i :: Natural -> Integer
n2i = toInteger

i2n :: Integer -> Maybe Natural
i2n i = if i >= 0
  then Just $ fromInteger i
  else Nothing

----------------------------------------

concatenateA :: (Applicative f) => (a -> f [b]) -> [a] -> f [b]
concatenateA f = traverse f >>> fmap concat

----------------------------------------

-- | 'coerce' a @newtype@ to its 'Unwrapped' value. 
coerceWrapped
  :: forall a.
    ( Wrapped a
    , Coercible a (Unwrapped a)
    )
  => a
  -> Unwrapped a
coerceWrapped = coerce

-- | 'coerce' an 'Unwrapped' value back to its @newtype@. 
coerceUnwrapped
  :: forall a.
    ( Wrapped a
    , Coercible a (Unwrapped a)
    )
  => Unwrapped a
  -> a
coerceUnwrapped = coerce

----------------------------------------
-- `text`

surround :: String -> String -> String -> String
surround l r s = l <> s <> r

-- braces :: String -> String
-- braces s = "{" <> s <> "}"

-- braces :: (IsString s) => s -> String
-- braces s = fromString "{" <> fromString s <> fromString s "}"

-- braces :: Text -> Text
-- braces t = T.pack "{" <> t <> T.pack "}"

char2text :: Char -> Text
char2text = T.singleton 

----------------------------------------
  
show' :: (Show a, StringConv String s) => a -> s
show' = show > toS

----------------------------------------

fromPredicate
  :: (a -> Bool)
  -> (a -> Maybe a)
fromPredicate p x =
  if   p x
  then Just x
  else Nothing 

either2validation' :: Either e a -> Validation (NonEmpty e) a
either2validation' = validationNel

either2validation :: Either e a -> Validation e a
either2validation = fromEither

validation2either :: Validation e a -> Either e a
validation2either = toEither

error2errors :: Validation e a -> Validation (NonEmpty e) a
error2errors = first (:|[])

-- maybe2first :: Maybe a -> First a
-- maybe2first = First

-- first2maybe :: First a -> Maybe a
-- first2maybe = getFirst

----------------------------------------

{-| builds a predicate from a disjunction of conjunctions of predicates.

-}
predicateDisjunctionOfConjunctions
  :: [[a -> Bool]]
  -> (a -> Bool)
predicateDisjunctionOfConjunctions predicates
  = \c -> any (all ($ c)) predicates

----------------------------------------

