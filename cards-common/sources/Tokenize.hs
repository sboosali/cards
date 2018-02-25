{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE OverloadedLists #-}

{-# LANGUAGE DeriveAnyClass #-}

{-| 

-}
module Tokenize where

import Enumerate (Enumerable)

--import qualified Data.Text.Lazy as T

import qualified Data.Set as Set  

import Prelude.Spiros 
import Data.List (groupBy)--, sortOn)

----------------------------------------

-- | e.g. @Tokenizer String = String -> [String]@
type Tokenizer t = t -> [t] -- 

{- | see 'tokenizeByCharacterCategory'

e.g.

@tokenize 'def'@

-}
tokenize :: TokenizeBy -> Tokenizer String
tokenize = \case
  TokenizeByCharacterCategoryIsolatingGroupingAndQuotation -> tokenizeByCharacterCategoryIsolatingGroupingAndQuotation

data TokenizeBy
 = TokenizeByCharacterCategoryIsolatingGroupingAndQuotation
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

-- | 'TokenizeByCharacterCategoryIsolatingGroupingAndQuotation'
instance Default TokenizeBy where
  def = TokenizeByCharacterCategoryIsolatingGroupingAndQuotation

{-|

calls @'groupBy' 'characterCategory'@, but further splits 'GroupingCharacter's into singletons. 


NOTE groups tokens by the custom 'CharacterCategory', not the Unicode 'GeneralCategory'.


examples:

>> tokenizeByCharacterCategoryIsolatingGroupingAndQuotation "e:al/en,be -e:al+be  year>=93 year<1996  o:\"First strike\" o:Flying (not (t:bird or t:dragon))"
["e", ":", "al", "/", "en", ", ", "be", " ", "-", "e", ":", "al", "+", "be", "  ", "year", ">=", "93", " ", "year", "<", "1996", "  ", "o", ":", "\"", "First", " ", "strike", "\"", " ", "o", ":", "Flying", " ", "(", "not", " ", "(", "t", ":", "bird", " ", "or", " ", "t", ":", "dragon", ")", ")"]



base cases:

>>> tokenizeByCharacterCategoryIsolatingGroupingAndQuotation ""
[]

>>> tokenizeByCharacterCategoryIsolatingGroupingAndQuotation [c]
[[c]]

-}
tokenizeByCharacterCategoryIsolatingGroupingAndQuotation
 :: String
 -> [String]
tokenizeByCharacterCategoryIsolatingGroupingAndQuotation
  = groupBy eqCharacterCategory
  > concatMap splitSingletonCharacters
  > filter (null > not)
  where
  splitSingletonCharacters :: String -> [String]
  neitherAreSingletonCharacters :: Char -> Char -> Bool

  splitSingletonCharacters
    = groupBy neitherAreSingletonCharacters

  -- check both chars against all predicates
  -- List monad
  neitherAreSingletonCharacters x y = all id $ do
    c <- ([x,y] :: [Char])
    p <- [ checkCharacterCategory GroupingCharacter
         , checkCharacterCategory QuotationCharacter
         ]
    let b = not (p c)
    return b

  -- isntGroupingCharacter
  --   = checkCharacterCategory GroupingCharacter > not

  -- (characterCategory -> GroupingCharacter
  -- predicate x y = eqCharacterCategory
  --  = all isntGroupingCharacter ([x,y] :: String)


----------------------------------------

{-|



> [' '..'\DEL']
" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\DEL"

> ascii = [' '..'\DEL']

> ascii & filter isPunctuation 
"!\"#%&'()*,-./:;?@[\\]_{}"

> ascii & filter isSymbol
"$+<=>^`|~"

> ascii & filter isSeparator
" "


-}
data CharacterCategory
 = AlphabeticCharacter
 | NumericCharacter
 | GroupingCharacter
 | QuotationCharacter
 | WhitespaceCharacter
 | SymbolicCharacter
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

checkCharacterCategory
 :: CharacterCategory
 -> Char
 -> Bool
checkCharacterCategory category
  = characterCategory
  > (== Just category)

eqCharacterCategory
 :: Char
 -> Char
 -> Bool
eqCharacterCategory = (==) `on` characterCategory

characterCategory :: Char -> Maybe CharacterCategory
characterCategory c
 | isAlpha      c = Just AlphabeticCharacter
 | isNumber     c = Just NumericCharacter
 | isGrouping   c = Just GroupingCharacter
 | isQuotation  c = Just QuotationCharacter
 | isWhitespace c = Just WhitespaceCharacter
 | isSymbolic   c = Just SymbolicCharacter
 | otherwise      = Nothing
 -- ^ NOTE the order (of the conditions) matters, as only the first few predicates are mutually exclusive, isSymbol being very broad

 where
   
 isGrouping = toCharacterPredicate
   ""
   [ClosePunctuation,OpenPunctuation]   -- `elem` "()[]{}"
   []

 isQuotation = toCharacterPredicate 
   "\""
   [InitialQuote,FinalQuote]
   []

 isWhitespace = toCharacterPredicate
   " \t\n\r\f"
   []
   [isSeparator]

 isSymbolic = toCharacterPredicate
   ""
   []
   [isSymbol, isPunctuation]
 
 -- isQuotation  = \c
 --   -> c `elem` "\""
 --   || generalCategory c `elem` [InitialQuote,FinalQuote]
 -- isWhitespace = (||) <$> isSeparator <*> (`elem` " \t\n\r\f")
 -- isSymbolic   = (||) <$> isSeparator <*> isPunctuation

{-|

@toCharacterPredicate characters categories predicates c@

is a character predicate that means:

* is @c@ any of the given @characters@?
* or does @c@ have any of the given unicode @categories@?
* or does @c@ satisfy any of the other given @predicates@?

e.g.


@isQuotation = toCharacterPredicate ['"'] [InitialQuote,FinalQuote] []@

-}
toCharacterPredicate
  :: [Char]
  -> [GeneralCategory]
  -> [Char -> Bool]
  -> (Char -> Bool)
toCharacterPredicate characters categories predicates c
 = any ($ c) allPredicates
 where
 allPredicates
   = characterPredicate
   : categoryPredicate
   : predicates
 
 characterPredicate
   = (Set.member&flip) characters'
 categoryPredicate
   = generalCategory
   > (Set.member&flip) categories'

 characters' = characters & Set.fromList
 categories' = categories & Set.fromList

-- characterCategorizer character' categories' predicates' c
--  = any ($ c) predicates
--  where
--  predicates = concat $
--    [ characters' & isCharacter 
--    , categories' & hasCategory 
--    , predicates'
--    ]
--  isCharacter characters = (`elem` characters)
--  hasCategory categories = generalCategory > (`elem` categories)

----------------------------------------
