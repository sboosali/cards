{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.Parser where

import MTGJSON.Extra hiding (try)
--import MTGJSON.Types
import MTGJSON.Known

import MTGJSON.Printer.Finite

import "parsers"  Text.Parser.Combinators as P
--import "parsers"  Text.Parser.Token       as P
import "parsers"  Text.Parser.Char        as P
import "trifecta" Text.Trifecta.Parser    as P

--import qualified Data.List.NonEmpty as NonEmpty

import Prelude (read)

----------------------------------------

type CardValidation = V CardErrors

type CardErrors = NonEmpty CardError

data CardError
 = MustBeNatural {-String-} Integer
 | MustBeInteger {-String-} Double
 | UnknownColor  {-String-} String
 | BadManaCost String
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable)
 --deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,NFData,Hashable,Enumerable)

{-

error $ "Invalid mana cost " <> show s <> ": " <> show err

-}

----------------------------------------

pManaCost :: (Integral i) => Parser (ManaCost i)
pManaCost = ManaCost <$> P.many pManaSymbol

{-|

the parsers use the printers via 'fromInjective',
but also are more lenient w.r.t. ordering. i.e.
they don't just parse the single canonical form
that's printed out, but any permutations too.

e.g. the following are all accepted by this parser:

@
-- phyrexian
{PU}
{UP}
{P/U}
{U/P}
...
@

@
-- monocolor hybrid
{2/U}
{U/2}
{2U}
{U2}
@

@
-- hybrid
{G/U}
{U/G}
...
@

@
-- variable
{X}
{Y}
{Z}
@

-}
pManaSymbol :: (Integral i) => Parser (ManaSymbol i)
pManaSymbol = between (char '{') (char '}') $
  choice
    [ try $ HybridSymbol     <$> pGuild
    
    , try $ MonoHybridSymbol <$>   ( (string "2/" *> pColor)
                                 <|> (pColor <* string "2/")
                                   ) --TODO Permutation

    , try $ PhyrexianSymbol  <$> (pColor <* string "/P")

    , try $ GenericSymbol    <$> (pNatural <&> fromIntegral)

    ,       ChromaSymbol     <$> pChroma
    ,       VariableSymbol   <$  oneOf "XYZ"
    ]

----------------------------------------

pGuild :: Parser Guild
pGuild = do
  mGuild <- toGuild <$> pColor <*> (char '/' *> pColor)
  case mGuild of
    Just guild -> return guild
    Nothing    -> fail "[pGuild] the two colors of a hybrid mana symbol must be different"
  --TODO monadic parser usage

pChroma :: Parser Chroma
pChroma = printer displayChroma

pColor :: Parser Color
pColor = printer displayColor
     -- chars
     -- [ 'W' $> White
     -- , 'U' $> Blue
     -- , 'B' $> Black
     -- , 'R' $> Red
     -- , 'G' $> Green
     -- ]

----------------------------------------

pNatural :: Parser  Natural
pNatural = pDigits <&> read --NOTE partial function is safely used

pDigits :: Parser  String  
pDigits = some digit

----------------------------------------
