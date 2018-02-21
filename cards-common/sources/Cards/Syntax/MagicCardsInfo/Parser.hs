
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}

{-| 


-}
module Cards.Syntax.MagicCardsInfo.Parser where

import Cards.Syntax.MagicCardsInfo.Types

--import Cards.Query.Types

-- import           Text.Megaparsec ()
-- import qualified Text.Megaparsec as P

import Text.Parsers.Frisby

import Prelude.Spiros hiding (P, (<>))
import Prelude (read)

----------------------------------------

freeform :: Text -> Syntax
freeform t = Syntax mciFreeText mciFields
 where
 mciFreeText = Just t
 mciFields   = []

----------------------------------------

prefixKeywords :: SyntaxTable ()
prefixKeywords = --  [ "", "", "" ]
  [ "o"          -: ()
  , "t"          -: ()
  , "cmc"        -: ()
  , "mana"       -: ()
  , "c"          -: ()
  , "ci"         -: ()
  , "in"         -: ()
  , "r"          -: ()
  , "pow"        -: ()
  , "tou"        -: ()
  , "e"          -: ()
  , "f"          -: ()
  , "year"       -: ()
  , "banned"     -: ()
  , "legal"      -: ()
  , "restricted" -: ()
  , "a"          -: ()
  , "is"         -: ()
  , "has"        -: ()
  , "l"          -: ()
  ]

prefixOperators :: SyntaxTable ()
prefixOperators =
  [ "!"          -: ()
  , "-"          -: ()
  , "not"        -: ()
  ]

infixOperators :: SyntaxTable ()
infixOperators =
  [ ":"          -: ()
  , "!"          -: ()
  , "="          -: ()
  , "<"          -: ()
  , ">"          -: ()
  , "<="         -: ()
  , ">="         -: ()
  , "or"         -: ()
  , "and"        -: ()
  ]

invalidNakedChars :: [Char]
invalidNakedChars =
  " :!\"()"

----------------------------------------

runAdditive :: String -> Natural
runAdditive s = runPeg additive s

runAdditive' :: String -> (Maybe Natural, String)
runAdditive' s = runPeg additive' s

additive' :: PM s (P s (Maybe Natural, String))
additive' = mdo
  p <- additive
  let q = (fmap Just (p <<- eof) // unit Nothing) <> rest
  return q

additive :: PM s (P s Natural)
additive = mdo
    additive <- newRule $ do
                  multitive <> char '+' ->> additive ## uncurry (+) // multitive
    multitive <- newRule $ do
                  primary <> char '*' ->> multitive ## uncurry (*) // primary
    primary <- newRule $ do
                  char '(' ->> additive <<- char ')' // decimal
    decimal <- newRule $ do
                  many1 (oneOf ['0' .. '9']) ## read
    return additive


----------------------------------------

{-NOTES






instance Applicative PE where
--the function (++), (,) ... but, 'text', etc, does this too
    mf <*> ma = PMap (\(f,a) -> f a) (Then mf ma)





mdo
 additive <- newRule $ do
      ((+) <$> multitive <*> char '+' *> additive)
   // multitive
 ...


mdo
 additive <- newRule $ do
      (multitive <> (char '+' *> additive) <&> uncurry (+))
   // multitive
 ...


mdo
 additive <- newRule $ do
      multitive <> char '+' ->> additive ## uncurry (+)
   // multitive
 ...



-- e.g.
additive = mdo
    additive <- newRule $ do
                  multitive <> char '+' ->> additive ## uncurry (+) // multitive
    multitive <- newRule $ do
                  primary <> char '*' ->> multitive ## uncurry (*) // primary
    primary <- newRule $ do
                  char '(' ->> additive <<- char ')' // decimal
    decimal <- newRule $ do
                  many1 (oneOf ['0' .. '9']) ## read
    return additive



(//) :: P s a -> P s a -> P s a infixl 1 
Ordered choice, try left argument, if it fails try the right one. This does not introduce any backtracking or penalty.


runPeg :: (forall s. PM s (P s a)) -> String -> a

Run a PEG grammar. Takes the rank-2 argument in order to ensure a rule created in one PM session isn't returned and used in another PEG parser.

There is no need for special error handling, as it can be trivially implemented via

 -- parse complete file, returning 'Nothing' if parse fails
 fmap Just (myParser <<- eof) // unit Nothing
There is also no need for the parser to return its unused input, as that can be retrieved via rest.

-- Now this returns (a,String) where String is the unconsumed input.
myParser <> rest



-}
