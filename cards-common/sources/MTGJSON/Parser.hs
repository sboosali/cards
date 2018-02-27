{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE DeriveAnyClass #-}

{-|

-}
module MTGJSON.Parser where

import MTGJSON.Extra hiding (try)
--import MTGJSON.Types
import MTGJSON.Known

import MTGJSON.Printer.Finite

import "parsers"  Text.Parser.Combinators 
import "parsers"  Text.Parser.Token       
import "parsers"  Text.Parser.Char        
--import "parsers" Text.Parser.Permutation

--import "trifecta" Text.Trifecta.Parser    as P

--import qualified Data.List.NonEmpty as NonEmpty

--import Prelude (read)

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

import qualified Text.ParserCombinators.ReadP as ReadP
import           Text.ParserCombinators.ReadP (ReadP)


-}

----------------------------------------

pManaCost
  :: forall p i. (Integral i)
  => (MonadFail p, TokenParsing p)
  => p (ManaCost i)
pManaCost = ManaCost <$> many pManaSymbol

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

>>> import Text.Trifecta.Parser (Parser,parseTest,runParser)
>>> p = parseTest (pManaSymbol :: Parser (ManaSymbol Natural))
>>> p' = runParser mempty (pManaSymbol :: Parser (ManaSymbol Natural))

>>> p "{PU}"
PhyrexianSymbol Blue

>>> p "{UP}"
PhyrexianSymbol Blue

>>> p "{P/U}"
PhyrexianSymbol Blue

>>> p "{U/P}"
PhyrexianSymbol Blue

>>> p "{2/U}"
MonoHybridSymbol Blue

>>> p "{U/2}"
MonoHybridSymbol Blue

>>> p "{2U}"
MonoHybridSymbol Blue

>>> p "{U2}"
MonoHybridSymbol Blue

>>> p "{U/G}"
HybridSymbol Simic

>>> p "{G/U}"
HybridSymbol Simic

>>> p "{X}"
VariableSymbol

>>> p "{Z}"
VariableSymbol

>>> p "{1}"
GenericSymbol 1

>>> p "{9}"
GenericSymbol 9

>>> p "{G/G}"
(interactive):1:3: error: expected: "}"
{G/G}<EOF> 
  ^        

-}
pManaSymbol
  :: forall p i. (Integral i)
  => (MonadFail p, TokenParsing p)
  => p (ManaSymbol i)
pManaSymbol = braces $  -- between (char '{') (char '}') $
  choice
    [ try $ HybridSymbol     <$> pGuild
                             <?> "hybrid symbol"
    
    , try $ PhyrexianSymbol  <$> pPhyrexian
                             <?> "phyrexian symbol"

    , try $ MonoHybridSymbol <$> pMonoHybrid
                             <?> "mono-hybrid symbol"

    , try $ GenericSymbol    <$> (natural <&> fromIntegral)
                             <?> "generic symbol"

    ,       ChromaSymbol     <$> pChroma
                             <?> "chroma symbol"
    
    ,       VariableSymbol   <$  oneOf "XYZ"
                             <?> "variable symbol"
    ]

----------------------------------------

pPhyrexian :: forall p. (CharParsing p) => p Color
pPhyrexian = pPermutedColor $ string "P"

pMonoHybrid :: forall p. (CharParsing p) => p Color
pMonoHybrid = pPermutedColor $ string "2"

pPermutedColor :: forall p. (CharParsing p) => p String -> p Color
pPermutedColor pSymbol
    = pColor  <* pSlash <* pSymbol
  <|> pSymbol *> pSlash *> pColor
 where
 pSlash = skipOptional $ char '/'
 --permute p

----------------------------------------

pGuild :: forall p. (MonadFail p, CharParsing p) => p Guild
pGuild = do
  guild' <- toGuild <$> pColor <*> (char '/' *> pColor)

  guild <- guild' & maybe failed return
  return guild

  where
  failed = unexpected expectation --TODO ignored
  expectation = "a hybrid mana symbol (the two colors must be different)"

  {-

  case guild' of
    Just guild -> return guild
    Nothing    -> fail
     "[pGuild] the two colors of a hybrid mana symbol must be different"
  --TODO monadic parser usage
  
  where
 -- pColors = 
 -}

pChroma :: forall p. (CharParsing p) => p Chroma
pChroma = printer displayChroma

pColor :: forall p. (CharParsing p) => p Color
pColor = printer displayColor
     -- chars
     -- [ 'W' $> White
     -- , 'U' $> Blue
     -- , 'B' $> Black
     -- , 'R' $> Red
     -- , 'G' $> Green
     -- ]

----------------------------------------

{-

pNatural :: Parser  Natural
pNatural = pDigits <&> read --NOTE partial function is safely used

pDigits :: Parser  String  
pDigits = some digit

-}

----------------------------------------

{-

import Text.Trifecta.Parser (parseTest,runParser)
p = parseTest (pManaSymbol :: Parser (ManaSymbol Natural)
p "{PU}"
p "{UP}"
p "{P/U}"
p "{U/P}"
p "{2/U}"
p "{U/2}"
p "{2U}"
p "{U2}"
p "{U/G}"
p "{G/U}"
p "{X}"
p "{Z}"


-}
