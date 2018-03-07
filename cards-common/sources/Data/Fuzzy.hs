
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-| simple configuration for fuzzily searching through strings.

-}
module Data.Fuzzy where

import "enumerate" Enumerate

import "spiros" Prelude.Spiros

----------------------------------------

{-| A query to be fuzzily-matched. 

See 'SearchConfig' and 'fuzzySearch'. 

-}
newtype Fuzzy = Fuzzy Text
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

wildcard :: Fuzzy
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
AmbiguousMatches ('a':|"bc")

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

@
>>> fuzzySearch def (Fuzzy "bc") (\(x,y) -> [x,y]) [("abc","xyz"), ("cba","zyz")]
UnambiguousMatch ("abc","xyz")
@

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

Calls @foldr@, like this (but not):

@
'foldr'
 :: (a -> MatchResult a -> MatchResult a)
 -> MatchResult a
 -> f a
 -> 'MatchResult' a
@

@foldr@, being right-associative i.e. "productive", may terminate on infinite input; but only if '_maxResults' is @('Just' n)@. 

-}
fuzzySearch
  :: forall a f. (Functor f, Foldable f)
  => SearchConfig
  -> Fuzzy
  -> (a -> Maybe [Text])
  -> f a
  -> MatchResult a
fuzzySearch SearchConfig{..} (Fuzzy query) munge candidates
  = candidates
  & fmap (id &&& munge)
  & foldr go []
  & reverse              -- `foldr` returns in "reverse" order
  & toMatchResult
  where
  go :: (a, Maybe [Text]) -> [a] -> [a]
  go (_, Nothing) xs = xs
  go (x, Just ts) xs = if all match' ts
    then (x:xs)
    else xs

  match' = fuzzyMatch _caseSensitivity _matchingStrategy query

-- -- internal FuzzySearchAccumulator
-- data Accumulator a = Accumulator !(Seq a) 

fuzzyMatch :: CaseSensitivity -> MatchingStrategy -> Text -> Text -> Bool
fuzzyMatch casing strategy tQuery tCandidate = _

{-# SPECIALIZE fuzzySearch :: SearchConfig -> Fuzzy -> (Text -> Maybe [Text]) -> [Text] -> MatchResult Text #-}

{-NOTES

foldr :: (a -> MatchResult a -> MatchResult a) -> (MatchResult a) -> f a -> (MatchResult a)

foldr :: (a -> b -> b) -> b -> t a -> b

Note that, since the head of the resulting expression is produced by an application of the operator to the first element of the list, foldr can produce a terminating expression from an infinite list.

-}

----------------------------------------
