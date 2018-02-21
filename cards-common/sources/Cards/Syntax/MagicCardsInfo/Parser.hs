
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}

{-| 

e.g.

@

Birds of Paradise
"Birds of Paradise"

!Anger
-- Match the full name

o:Flying
o:"First strike"
o:{T} o:"add one mana of any color"

o:"whenever ~ deals combat damage"
-- `~` is an alias for the card name

t:angel
t:"legendary angel"
t:basic
t:"arcane instant"

c:w
-- Any card that is white

c:wu
-- Any card that is white or blue

c:wum
-- Any card that is white or blue, and multicolored

c!w
-- Cards that are only white

c!wu
-- Cards that are only white or blue, or both

c!wum
-- Cards that are only white and blue, and multicolored

c!wubrgm
-- Cards that are all five colors

c:m
-- Any multicolored card

c:l or c:c
-- Lands and colorless cards


ci:wu
-- Any card that is white or blue, but does not contain any black, red or green mana symbols

in:wu
-- Any card that is white or blue according to the color indicator.

mana=3G
-- Spells that cost exactly 3G, or split cards that can be cast with 3G

mana>=2WW
-- Spells that cost at least two white and two colorless mana

mana<GGGGGG
-- Spells that can be cast with strictly less than six green mana

mana>=2RR mana<=6RR
-- Spells that cost two red mana and between two and six colorless mana

mana>={2/R}
mana>={W/U}
mana>={UP}

pow<=1
tou<pow
pow>=cmc
cmc=7
cmc>=*

r:common
r:uncommon
r:rare
r:mythic

f:standard
f:block
f:extended
f:vintage
f:classic
f:legacy
f:modern
f:commander

banned:legacy
legal:standard
restricted:vintage

a:"Quinton Hoover"
a:Guay

e:al/en
-- Uses the abbreviations that are listed on the sitemap

e:al,be
-- Cards that appear in Alpha or Beta

e:al+be
-- Cards that appear in Alpha and Beta

e:al,be -e:al+be
-- Cards that appear in Alpha or Beta but not in both editions

year<=1995
-- Cards printed in 1995 and earlier

is:split
is:flip
is:vanilla
-- Creatures with no card text

is:old
is:new
is:future
-- Old/new/future card face

is:timeshifted
is:funny
not:funny
-- Unglued/Unhinged/Happy Holidays Promos

is:promo
-- Promotional cards

is:promo is:old
-- Promotional cards with the original card face

is:permanent
is:spell

is:black-bordered
is:white-bordered
is:silver-bordered

has:foil

l:de
l:it
l:jp

o:"whenever ~" ((o:"deals damage to a" or o:"deals combat damage to a") (o:opponent or o:player)) or o:"attacks and isn't blocked")

@

e.g.

@
_ and _
_ _
-- i.e. whitespace

_ or _

not _
- _

( _ )
@


-}
module Cards.Syntax.MagicCardsInfo.Parser where

import Cards.Syntax.MagicCardsInfo.Types

--import Cards.Query.Types

-- import           Text.Megaparsec ()
-- import qualified Text.Megaparsec as P

import qualified Text.Parsers.Frisby as Frisby
import           Text.Parsers.Frisby hiding (text, (<>))

import Enumerate
import Enumerate.Function

import qualified Data.Text.Lazy as T

import qualified Data.Map as Map

import Prelude.Spiros hiding (P)
import Prelude (read)

----------------------------------------

braces :: Text -> Text
braces t = "{" <> t <> "}"

char2text :: Char -> Text
char2text = T.singleton 

show' :: (Show a, StringConv String s) => a -> s
show' = show > toS

----------------------------------------
  
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

{-|

Parses a keyword (or a set of aliases for that keyword),
with the given separator (or, variations for the separator),
and the given parser.

e.g.

@
cmc:3
@

is parsed by

@
attribute ["cmc", ...] [":", ...] number
-- where number :: P s Numeric
@


-}

attribute :: [Text] -> [Text] -> P s Text -> P s Attribute
attribute keywords seperators pConstraint = a
  where
  -- "subject / verb / object"
  s = texts keywords
  v = texts seperators
  o = quotable pConstraint
  a = Attribute <$> s <*> v <*> o

-- attribute :: [Text] -> SyntaxTable a -> P s Text -> P s Attribute
-- attribute keywords seperators pConstraint = p
--   where
--   v = quotable pConstraint
--   o = vocabulary seperators
--   k = texts keywords
--   p = Attribute <$> k <*> o <*> v

-- pAttribute :: [Text] -> P s Text -> P s Attribute
-- pAttribute ks pConstraint = do
--   v <- pQuotable pConstraint
--   k <- rule$ texts ks
--   p <- rule$
--     Attribute <$> (k <* char ':') <*> v
--   return p

----------------------------------------
  
runMagicCardsInfo' :: String -> (Complete Syntax)
runMagicCardsInfo' = runPeg gMagicCardsInfo' 

gMagicCardsInfo' :: PM s (P s (Complete Syntax))
gMagicCardsInfo' = complete gMagicCardsInfo

runMagicCardsInfo :: String -> Syntax
runMagicCardsInfo = runPeg gMagicCardsInfo 

----------------------------------------


----------------------------------------



----------------------------------------

allPrefixKeywords :: SyntaxTable ()
allPrefixKeywords = 
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
  , "l"          -: ()
  , "is"         -: ()
  , "not"        -: ()
  , "has"        -: ()
  ]

allUnaryPrefixOperators :: SyntaxTable ()
allUnaryPrefixOperators =
  [ "!"          -: ()
  , "-"          -: ()
  , "not"        -: ()
  ]

allInfixOperators :: SyntaxTable ()
allInfixOperators =
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

-- | for @"is"@ and @"not"@.
identityKeywords :: [Text]
identityKeywords =
 [ "vanilla" 
 , "permanent"
 , "spell"

 , "split"
 , "flip"

 , "new"
 , "old"
 
 , "future"
 , "timeshifted"

 , "funny"
 , "promo"
 
 , "black-bordered"
 , "white-bordered"
 , "silver-bordered"
 ]

-- | for @"has"@.
possessionKeywords :: [Text]
possessionKeywords =
  [ "foil"
  ]

----------------------------------------
  
numericKeywords :: [Text]
--tsNumeric
numericKeywords = 
  [ "cmc"
  , "pow"
  , "tou"
  ]

colorKeywords :: [Text]
colorKeywords = 
  [ "c"
  , "ci"
  , "in"
  ]

colorValues :: [Text]
colorValues =
  [
  ]

pNumericKeywords :: P s Text
pNumericKeywords = texts numericKeywords

pColorKeywords :: P s Text
pColorKeywords = texts colorKeywords

---------------------------------------

genericOperators :: SyntaxTable (GenericComparator)
genericOperators =
  [ ":"          -: Has
  , "!"          -: Is
  ]

numericOperators :: SyntaxTable (NumericComparator)
numericOperators =
  [ ":"          -: Equals -- HAS
  , "!"          -: Equals -- IS
  , "="          -: Equals
  , "<"          -: Lesser
  , ">"          -: Greater
  , "<="         -: LesserEquals
  , ">="         -: GreaterEquals
  ]

-- booleanPrefixes :: SyntaxTable (GenericComparator)
-- booleanPrefixes =
--   [ "is"          -: Is
--   , "not"         -: Isnt
--   ]

-- numericOperators :: SyntaxTable (Comparison Numeric)
-- numericOperators =
--   [ ":"          -: (Contains)
--   , "!"          -: (Equals)
--   , "="          -: (Equals)
--   , "<"          -: (Lesser)
--   , ">"          -: (Greater)
--   , "<="         -: (LEQ)
--   , ">="         -: (GEQ)
--   ]

pGenericOperators :: P s GenericComparator
pGenericOperators = aliases genericOperators

pNumericOperators :: P s NumericComparator
pNumericOperators = aliases numericOperators

----------------------------------------

pNumericComparison :: P s Attribute
pNumericComparison = _ -- pAttribute' pNumeric

pNumeric :: P s (Numeric i)
pNumeric = _

pIs :: P s Attribute
--pIs :: Grammar Attribute --gIdentity
pIs = attribute ["is", "not"] [":"] (texts identityKeywords)

pHas :: P s Attribute
pHas = attribute ["has"] [":"] (texts possessionKeywords)

---------------------------------------

displayManaCost :: (Show i) => Pretty (ManaCost i)
displayManaCost = \case
  ManaCost mana -> mana & maybe "" displayManaSymbols

displayManaSymbols :: (Show i) => Pretty (ManaSymbols i)
displayManaSymbols (ManaSymbols symbols) = t
    where
    t  = ts & T.intercalate ""         -- e.g. "{2}{U}{G}"
    ts = symbols <&> displayManaSymbol -- e.g. ["{2}","{U}","{G}"]

displayManaSymbol :: (Show i) => Pretty (ManaSymbol i)
displayManaSymbol = \case
  GenericSymbol      i         -> displayGenericManaCost i
  HueSymbol          hue       -> displayHue             hue
  HybridSymbol       hybrid    -> displayHybrid          hybrid
  PhyrexianSymbol    phyrexian -> displayPhyrexian       phyrexian

displayGenericManaCost :: (Show i) => Pretty i
displayGenericManaCost = show > toS > braces

displayHue :: Pretty Hue
displayHue = hue2letter > char2text > braces

-- \case
--   TrueColor c -> displayColor c
--   Colorless c -> "{C}"

-- displayColor :: Pretty Color
-- displayColor = color2letter > braces

displayHybrid :: (Show i) => Pretty (Hybrid i)
displayHybrid = \case
  GuildHybrid  guild -> displayGuildHybrid guild
  GrayHybrid i color -> displayGrayHybrid i color

displayGuildHybrid :: Pretty Guild
displayGuildHybrid
  = fromGuild'
  > fmap (color2text) -- e.g. ["U","G"]
  > T.intercalate "/" -- e.g. "U/G"
  > braces            -- e.g. "{U/G}"
  --displayColorHybrid colors = (colors & fromGuild) & 

displayGrayHybrid :: (Show i) => i -> Color -> Text
displayGrayHybrid i c = displayRawHybrid i' c'
  where
  i' = show' i
  c' = char2text (color2letter c)

displayRawHybrid :: Text -> Text -> Text
displayRawHybrid x y = "{" <> t <> "}"
  where
  t = x <> "/" <> y -- [x,y]

displayPhyrexian :: Pretty Phyrexian
displayPhyrexian = \case
  Phyrexian c -> "{P" <> t <> "}"
                     where
                     t = char2text (color2letter c)

---------------------------------------

-- | in ascending (canonical, i.e. WUBRG) order
fromGuild' :: Guild -> [Color]
fromGuild' = fromGuild > pair2list > sort
 where
 pair2list (x,y) = [x,y]

fromGuild :: Guild -> (Color,Color)
fromGuild = \case
 Azorius  -> (White, Blue)
 Dimir    -> (Blue,  Black)
 Rakdos   -> (Black, Red)
 Gruul    -> (Red,   Green)
 Selesnya -> (Green, White)
 Orzhov   -> (White, Black)
 Golgari  -> (Black, Green)
 Simic    -> (Green, Blue)
 Izzet    -> (Blue,  Red)
 Boros    -> (Red,   White)

hue2text :: Hue -> Text
hue2text = hue2letter > char2text

color2text :: Color -> Text
color2text = color2letter > char2text

hue2letter :: Hue -> Char
hue2letter = \case
  TrueColor color -> color2letter color
  Colorless       -> 'C'

color2letter :: Color -> Char
color2letter = \case
   White -> 'W'
   Blue  -> 'U'
   Black -> 'B'
   Red   -> 'R'
   Green -> 'G'

---------------------------------------
-- Defined inverted for verifying surjectivity via pattern matching, can be inverted again.

-- parseIs' :: Text -> Is
-- parseIs' = invert' displayIs
-- --Map Text Is’

displayIs :: Is -> Text
displayIs = \case
 IsFace      face      -> face      & displayFace
 IsFrame     frame     -> frame     & displayFrame
 IsBorder    border    -> border    & displayBorder
 IsPredicate predicate -> predicate & displayPredicate

displayFace :: Face -> Text
displayFace = \case
 NormalFace       -> "normal"
 DoubleFace       -> "double"
 SplitFace        -> "split"
 FlipFace         -> "flip"
 
displayFrame :: Frame -> Text
displayFrame = \case
 OldFrame         -> "old"
 TimeshiftedFrame -> "timeshifted"
 NewFrame         -> "new"
 FutureFrame      -> "future"

displayBorder :: Border -> Text
displayBorder = \case
 BlackBordered    -> "black"
 WhiteBordered    -> "white"
 SilverBordered   -> "silver"

displayPredicate :: KnownPredicate -> Text
displayPredicate = \case
 Spell            -> "spell"
 Permanent        -> "permanent"
 Vanilla          -> "vanilla"

----------------------------------------

invert' :: (Enumerable a, Ord a, Ord b) => (a -> b) -> (Map b a)
invert' = fromFunction > invertMap > _ -- Map.

----------------------------------------

gMagicCardsInfo :: PM s (P s Syntax)
gMagicCardsInfo = mdo
    p <- rule $ _
    return p

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
