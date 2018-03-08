
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| simple configuration for fuzzily searching through strings.

-}
module Data.Fuzzy where

import "enumerate" Enumerate

import qualified Data.Text as T
import           Data.Text (Text)

--[List-Not-Seq] import qualified Data.Sequence as Seq

import "spiros" Prelude.Spiros hiding (Text)

----------------------------------------

{-| A query to be fuzzily-matched. 

See 'SearchConfig' and 'fuzzySearch'. 

-}
newtype Query = Query Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

----------------------------------------

-- newtype SearchConfig = SearchConfig (Set ())
-- data SearchOption = 

{-|

Example usage:

@
'def' { '_matchResults' = 'Just' 10 }
@

Top-level search options:

* '_caseSensitivity': (see 'CaseSensitivity'). 
* '_matchingStrategy': (see 'MatchingStrategy'). 
* '_maxResults': The maximum number of results. Searching short-circuits if that many are matched, returning those matches. @Nothing@ means "unbounded" a.k.a. "keep searching, finding as many matches as exist". 

-}
data SearchConfig = SearchConfig
 { _caseSensitivity  :: CaseSensitivity
 , _matchingStrategy :: MatchingStrategy
 , _maxResults       :: Maybe Natural    -- ^ unbounded by default
 } deriving (Show,Read,Eq,Ord,Generic)

instance NFData     SearchConfig
instance Hashable   SearchConfig

-- | @= 'defaultSearchConfig'@
instance Default SearchConfig where def = defaultSearchConfig

{- | the pointwise 'def'aults:

@= SearchConfig 'def' 'def' Nothing@

also see:

* 'loosestSearchConfig'
* 'strictestSearchConfig'

-}
defaultSearchConfig :: SearchConfig
defaultSearchConfig = SearchConfig def def Nothing

----------------------------------------

{-|

Isomorphic to @[a]@ (see 'toMatchResult'),
but with more descriptive naming/casing. 

-}
data MatchResult a
 = NoMatch
 | UnambiguousMatch a
 | AmbiguousMatches (NonEmpty a)
 deriving (Functor,Foldable,Traversable,Show,Read,Eq,Ord,Generic)

instance (NFData a)     => NFData     (MatchResult a)
instance (Hashable a)   => Hashable   (MatchResult a)
--instance (Enumerable a) => Enumerable (MatchResult a)

{-| 'toMatchResult' / 'fromMatchResult'. 
-}
instance IsList (MatchResult a) where
  type Item (MatchResult a) = a
  toList   = fromMatchResult
  fromList = toMatchResult

----------------------------------------

{-| 'CaseInsensitive' is strictly looser than 'CaseSensitivity'. 

-}
data CaseSensitivity
 = CaseSensitive
 | CaseInsensitive -- ^ the default
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic)

instance NFData     CaseSensitivity
instance Hashable   CaseSensitivity
instance Enumerable CaseSensitivity

-- | @= 'CaseInsensitive'@
instance Default CaseSensitivity where def = CaseInsensitive

----------------------------------------

{-| The strategy by which a query is compared to each candidate. 

Each strategy is looser than any higher strategy (i.e. 'MatchExactly' is the strictest, 'MatchAlphabet' is the loosest). 

The cases:

* 'MatchExactly':     Match the query against the whole candidate string; case-sensitivity is a separate option, specified by 'CaseSensitivity'. 
* 'MatchPrefix':      Match the query against any prefix of the candidate string. 
* 'MatchAffix':       Match the query against any affix of the candidate string; i.e. any prefix, suffix, or infix).  
* 'MatchSubsequence': Matches if the query is a subsequence of the candidate; i.e. 
* 'MatchSubset':      Matches if every character in the query is contained by the candidate; if the query has multiples of a particular character, it must be present that many times or more in the candidate; order doesn't matter, and both the query and candidate could be re-ordered without affecting the result. 
* 'MatchAlphabet':    like 'MatchSubset', but multiples don't matter. 

Given the list @["a","ab","bd","bd...","...bd,".b.d.",".d.b.","bb.dd"]@ to search...

* @'MatchExactly':     @ the query @"bd"@ matches @"bd"@ only (i.e. @["bd"]@)
* @'MatchPrefix':      @ the query @"bd"@ matches @"bd..."@ too (i.e. @["bd","bd..."]@)
* @'MatchAffix':       @ the query @"bd"@ matches @"...bd"@ too (i.e. @["bd"","bd...","...bd"]@)
* @'MatchSubsequence': @ the query @"bd"@ matches @".b.d."@ too (i.e. @["bd"","bd...","...bd",".b.d."]@)
* @'MatchSubset':      @ the query @"bd"@ matches @".d.b."@ too (i.e. @["bd"","bd...","...bd",".b.d.",".d.b."]@)
* @'MatchAlphabet':    @ the query @"bd"@ matches @"bb.dd"@ too (i.e. @["bd"","bd...","...bd",".b.d.",".d.b.","bb.dd"]@)

and no strategy matches @"bd"@ against @"ab"@ or @""@.

-}
data MatchingStrategy
 = MatchExactly     -- ^ 
 | MatchPrefix      -- ^ 
 | MatchAffix       -- ^ the 'Default' matching strategy for fuzzy search. 
 | MatchSubsequence -- ^ 
 | MatchSubset      -- ^ 
 | MatchAlphabet    -- ^ 
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic)

instance NFData     MatchingStrategy
instance Hashable   MatchingStrategy
instance Enumerable MatchingStrategy

-- | @= 'MatchAffix'@
instance Default MatchingStrategy where def = MatchAffix

----------------------------------------

wildcard :: Query
wildcard = ""

----------------------------------------

{-|

-}
fromMatchResult :: MatchResult a -> [a]
fromMatchResult = \case
  NoMatch                  ->  []    
  UnambiguousMatch x       -> [x]
  AmbiguousMatches (x:|xs) -> (x:xs)

{-|

>>> toMatchResult ""
NoMatch
>>> toMatchResult "a"
UnambiguousMatch 'a'
>>> toMatchResult "abc"
AmbiguousMatches ('a' :| "bc")

-}
toMatchResult :: [a] -> MatchResult a
toMatchResult = \case
  []     -> NoMatch
  [x]    -> UnambiguousMatch x
  (x:xs) -> AmbiguousMatches (x:|xs)

----------------------------------------

-- | @= SearchConfig 'CaseInensitive' 'MatchAlphabet' Nothing@
loosestSearchConfig :: SearchConfig
loosestSearchConfig = SearchConfig CaseInsensitive MatchAlphabet Nothing

-- | @= SearchConfig 'CaseSensitive' 'MatchExactly' (Just 1)@
strictestSearchConfig :: SearchConfig
strictestSearchConfig = SearchConfig CaseSensitive MatchExactly (Just 1)

----------------------------------------

{-|

Parameters:

@
fuzzySearch config query munge candidates
@

* @munge@ extract a disjunction of matchable text; i.e. if any of the @text@s in the @list@ match, then the whole item matches. 

Example usage:

>>> :set -XOverloadedStrings
>>> fuzzySearch def (Query "_") (\(x,y) -> Just [x,y]) [("abc","xyz"), ("",""), ("cba","zyx")]
NoMatch
>>> fuzzySearch def (Query "bc") (\(x,y) -> Just [x,y]) [("abc","xyz"), ("",""), ("cba","zyx")]
UnambiguousMatch ("abc","xyz")
>>> fuzzySearch def (Query "z") (\(x,y) -> Just [x,y]) [("abc","xyz"), ("",""), ("cba","zyx")]
AmbiguousMatches (("abc","xyz") :| [("cba","zyx")])
>>> fuzzySearch def{ _maxResults = Just 1} (Query "z") (\(x,y) -> Just [x,y]) [("abc","xyz"), ("",""), ("cba","zyx")]
UnambiguousMatch ("abc","xyz")
>>> fuzzySearch def (Query "BC") (\(x,y) -> Just [x,y]) [("abc","xyz"), ("",""), ("cba","zyx")]
UnambiguousMatch ("abc","xyz")
>>> fuzzySearch def{ _caseSensitivity = CaseSensitive } (Query "BC") (\(x,y) -> Just [x,y]) [("abc","xyz"), ("",""), ("cba","zyx")]
NoMatch
>>> fuzzySearch def{ _matchingStrategy = MatchExactly } (Query "ab") (\(x,y) -> Just [x,y]) [("abc","xyz"), ("",""), ("cba","zyx")]
NoMatch
>>> fuzzySearch def{ _matchingStrategy = MatchPrefix } (Query "ab") (\(x,y) -> Just [x,y]) [("abc","xyz"), ("",""), ("cba","zyx")]
UnambiguousMatch ("abc","xyz")

Some laws:

* output @length@:

@
ys = fuzzySearch _ _ _ xs
-->
length ys <= length xs
@

* @maxResults@:

@
ys = fuzzySearch def{_maxResults = Just k} _ _ xs
-->
length ys <= k
@

* filtering

@
[] = fuzzySearch _ _ (const $ Nothing) xs
xs = fuzzySearch _ _ (const $ Just []) xs

-- when @munge _ = Nothing@, everything is filtered away, and thus nothing gets matched. 
-- when @munge _ = []@, nothing is extracted, no match can fail, and thus everything gets matched. 
@

Notes:

Calls 'fuzzyMatch' on each @Text@ of each @munge@d item.

Calls @foldr@, like this (but not):

@
'foldr'
 :: (a -> MatchResult a -> MatchResult a)
 -> MatchResult a
 -> f a
 -> 'MatchResult' a
@

@foldr@, being right-associative i.e. "productive", may terminate on infinite input; but only if the consumer is lazy enough too.

TODO use 'apo'? Works when '_maxResults' is @('Just' n)@. 

-}
fuzzySearch
  :: forall a f. (Functor f, Foldable f)
  => SearchConfig
  -> Query
  -> (a -> Maybe [Text])
  -> f a
  -> MatchResult a
fuzzySearch SearchConfig{..} query munge candidates
  = candidates
  & fmap (id &&&! munge)
  & foldr go []
  & shrink -- toList              -- with list cons, `foldr` would return in "reverse" order?
  & toMatchResult
  where
  --[List-Not-Seq] go :: Pair a (Maybe [Text]) -> Seq a -> Seq a
  go :: Pair a (Maybe [Text]) -> List a -> List a  
  
  go (Pair _ Nothing)   xs = xs
  go (Pair x (Just ts)) xs =
    {- let
    isSmallOrUnbounded = _maxResults' & maybe True (\n -> Seq.length xs `lessThan` n)
    doesAnyPartMatch   = any match' ts
    in-}
    
    -- to append something: [1] the matches must be small enough, and [2] the query must match it. 
    --[List-Not-Seq] if isSmallOrUnbounded && doesAnyPartMatch
    if any match' ts
    then x:xs  --[List-Not-Seq] (x Seq.<| xs) -- unlike `[]`, `Seq` has constant-time left-cons. 
    else xs

  --[List-Not-Seq] _maxResults' = _maxResults <&> fromIntegral

  shrink = _maxResults & maybe id (\(fromIntegral -> n) -> take n)
  
  match' = fuzzyMatch _caseSensitivity _matchingStrategy query

{-# SPECIALIZE fuzzySearch :: SearchConfig -> Query -> (Text -> Maybe [Text]) -> [Text] -> MatchResult Text #-}

-- -- internal FuzzySearchAccumulator
-- data Accumulator a = Accumulator !(Seq a) 

fuzzyMatch :: CaseSensitivity -> MatchingStrategy -> Query -> Text -> Bool
fuzzyMatch casing strategy (Query query) = \candidate ->
  let t = fCase candidate
  in q `fMatch` t

  where
  q = fCase query

  fCase  = fromCaseSensitivity  casing
  fMatch = fromMatchingStrategy strategy

fromCaseSensitivity :: CaseSensitivity -> (Text -> Text)
fromCaseSensitivity = \case
  CaseSensitive   -> id
  CaseInsensitive -> T.toCaseFold

fromMatchingStrategy :: MatchingStrategy -> Text -> (Text -> Bool)
fromMatchingStrategy strategy query = predicate

 where
 predicate = go strategy

 go = \case
   MatchExactly     -> (==)         query
   MatchPrefix      -> T.isPrefixOf query
   MatchAffix       -> T.isInfixOf  query
   MatchSubsequence -> __ERROR__ "TODO" -- _ -- T.
   MatchSubset      -> __ERROR__ "TODO" -- _ -- T.
   MatchAlphabet    -> __ERROR__ "TODO" -- _ -- T.

{-
 where
 predicate = go strategy query

 go = \case
   MatchExactly     -> (==) 
   MatchPrefix      -> T.isPrefixOf
   MatchAffix       -> T.isInfixOf
   MatchSubsequence -> _ -- T.
   MatchSubset      -> _ -- T.
   MatchAlphabet    -> _ -- T.
-}

----------------------------------------

-- | strict pair
data Pair a b = Pair {-# UNPACK #-} !a {-# UNPACK #-} !b

(&&&!) :: (a -> b) -> (a -> c) -> a -> Pair b c
f &&&! g = \x -> Pair (f x) (g x)
    
----------------------------------------
{-NOTES



APO

apo
 :: (a -> Base t (Either t a))
 -> (a -> t)


ELGOT

elgot :: Functor f => (f a -> a) -> (b -> Either a (f b)) -> b -> a

elgot @Identity :: (Identity a -> a) -> (b -> Either a (Identity b)) -> b -> a

simpleElgot :: forall a b. (b -> Either a b) -> (b -> a)
simpleElgot f = elgot getIdentity (f >>> Identity)

simpleElgot (\_ -> Left a) _ = a


TAKE

simpleElgot @[a] @[a] :: ([a] -> Either [a] [a]) -> ([a] -> [a])

simpleElgot @[a] @(Natural,[a]) :: ((Natural,[a]) -> Either [a] (Natural,[a])) -> ((Natural,[a]) -> [a])

take :: Natural -> ([a] -> [a])
take k xs = reverse ys
 --NOTE the `reverse` is necessary,
 -- because foldr-style recursion appends items to the accumulator in a queue-like way. 

 where
 (_,_,ys) = simpleElgot go initial

 initial :: (Natural,[a],[a])
 initial = (k, xs, [])

 go :: (Natural,[a],[a]) -> Either [a] (Natural,[a],[a])
 go (0,  _,     ys) = Left ys -- halt if output got long enough
 go (_, [],     ys) = Left ys -- halt if input is too short
 go (i, (x:xs), ys) = Right (i-1, xs, (x:ys)) -- otherwise, append and continue


 go :: Natural -> [a] -> [a] -> Either [a] (Natural,[a],[a])

type TakeAccumulator a = HList [Natural, [a], Seq a]




FOLDR

foldr :: (a -> MatchResult a -> MatchResult a) -> (MatchResult a) -> f a -> (MatchResult a)

foldr :: (a -> b -> b) -> b -> t a -> b

Note that, since the head of the resulting expression is produced by an application of the operator to the first element of the list, foldr can produce a terminating expression from an infinite list.

-}

----------------------------------------
