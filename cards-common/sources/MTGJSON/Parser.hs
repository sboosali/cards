{-# LANGUAGE ApplicativeDo #-}
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

import "trifecta" Text.Trifecta as P

--import qualified Data.List.NonEmpty as NonEmpty

--import Prelude (read)

----------------------------------------

parseColorChar :: Parse Color
parseColorChar = print2parse displayColorChar

parseColorWord :: Parse Color
parseColorWord = print2parse displayColorWord

--   :: Parse [Color]
-- parseColorsAsChars
--   = _

-- parseColorsAsWords
--   :: Parse [Color]
-- parseColorsAsWords
--   = _

----------------------------------------

  
{-|

see 'parseOracle'

-}
parseOracleLoosely :: String -> Oracle
parseOracleLoosely s
  = s
  & parseOracle
  & fromMaybe (OracleVerbatim t)
  where
  t = toS s

{-|


e.g. @Ballynock Trapper@

oracle text:

@
{T}: Tap target creature.
Whenever you cast a white spell, you may untap Ballynock Trapper.
@

a.k.a.

@
{T}: Tap target creature.
Whenever you cast a white spell, you may untap ~.
@

doctests:

@
-- with 'IsList' and 'IsString' sugar:
 
>>> :set -XOverloadedLists
>>> :set -XOverloadedStrings
>>> oracleBallynockTrapper = [['OracleSymbol' "{T}", ": Tap target creature."], ["Whenever you cast a white spell, you may untap ", 'OracleNamesake', "."]] :: Oracle

-- with explicit constructors:

>>> oracleBallynockTrapper_explicit = 'OracleFrames' (('OracleFrame' ['OracleParagraph' ['OracleSymbol' "{T}", 'OraclePhrase' ": Tap target creature."], 'OracleParagraph' ['OraclePhrase' "Whenever you cast a white spell, you may untap ", 'OracleNamesake', 'OraclePhrase' "."]]) :|[])

>>> oracleBallynockTrapper_explicit == oracleBallynockTrapper
True

>>> parseOracle "{T}: Tap target creature.\n"Whenever you cast a white spell, you may untap Ballynock Trapper." == oracleBallynockTrapper
True


e.g. @'vanilla'@

>>> :set -XOverloadedLists
>>> [] == vanilla
True
>>> :set -XOverloadedStrings
>>> "" == vanilla
True



-}
parseOracle :: Parse Oracle
parseOracle 
  = P.parseString pOracle mempty
  > result2maybe

----------------------------------------

{-|


-}
pOracle
  :: forall p.
    ( TokenParsing p
    , MonadFail p
    )
  => p Oracle
pOracle = pOracleFrame <&> (:|[]) <&> OracleFrames --TODO levelers

{-|


-}
pOracleFrame
  :: forall p.
    ( TokenParsing p
    , MonadFail p
    )
  => p OracleFrame
pOracleFrame = lineSep pOracleParagraph
  <&> OracleFrame

{-|


-}
pOracleParagraph
  :: forall p.
    ( TokenParsing p
    , MonadFail p
    )
  => p OracleParagraph
pOracleParagraph = many pOracleChunk
  <&> OracleParagraph

{-|


-}
pOracleChunk
  :: forall p.
    ( TokenParsing p
    , MonadFail p
    )
  => p OracleChunk
pOracleChunk = choice
  [ try $ OracleNamesake <$  pOracleNamesake
  , try $ OracleSymbol   <$> pOracleSymbol
  , try $ OraclePhrase   <$> pOraclePhrase
  ]

----------------------------------------

{-|


-}
pOraclePhrase
  :: forall p.
    ( TokenParsing p
    )
  => p Text
pOraclePhrase = p <&> fromString -- T.pack
 where
 p    = good `manyTill` bad
 good = satisfy (not . isSpace)
 -- anyChar 
 bad  = try (satisfy isOracleReservedCharacter)
 -- (satisfy isOracleReservedCharacter)

{-|


-}
pOracleNamesake
  :: forall p.
    ( TokenParsing p
    )
  => p ()
pOracleNamesake = () <$ go
 where
 go = choice
     [ string "~"  -- char '~'
     , string "CARDNAME"
     ]

----------------------------------------

{-|


-}
pOracleSymbol
  :: forall p.
    ( TokenParsing p
    , MonadFail p
    )
  => p OracleSymbol
pOracleSymbol = braces pOracleSymbol'


{-|


-}
pOracleSymbol'
  :: forall p.
    ( TokenParsing p
    , MonadFail p
    )
  => p OracleSymbol
pOracleSymbol' = choice
  [ try $ Right <$> pKnownSymbol
  , try $ Left  <$> pUnknownSymbol' 
  ]

{-|


-}
pUnknownSymbol'
  :: forall p.
    ( TokenParsing p
    )
  => p Text
pUnknownSymbol'
  = f <$> some1 (satisfy isOracleSymbolCharacter)
  where
  f = toList > fromString -- T.pack

{-|


-}
pKnownSymbol
  :: forall p.
    ( TokenParsing p
    , MonadFail p
    )
  => p KnownSymbol
pKnownSymbol = choice
  [ try $ ManaSymbol <$> pKnownManaSymbol'
  , try $ MiscellaneousSymbol <$> pMiscellaneousSymbol' 
  ]
  where
  pKnownManaSymbol' = pManaSymbol' :: p KnownManaSymbol


{-|


-}
pMiscellaneousSymbol'
  :: forall p.
    ( TokenParsing p
    , MonadFail p
    )
  => p MiscellaneousSymbol
pMiscellaneousSymbol' = choice
 [ TapSymbol   <$ string "T"
 , UntapSymbol <$ string "Q"
 ]

----------------------------------------

theOracleReservedTokens :: [String]
theOracleReservedTokens
   = theOracleReservedKeywords
  ++ (theOracleReservedCharacters & fmap (:[]))

theOracleReservedKeywords :: [String]
theOracleReservedKeywords =
  [ "CARDNAME"
  ]

theOracleReservedCharacters :: [Char]
theOracleReservedCharacters =
  "~{}"

isOracleReservedKeyword :: String -> Bool
isOracleReservedKeyword = (`elem` theOracleReservedKeywords)

isOracleReservedCharacter :: Char -> Bool
isOracleReservedCharacter = (`elem` theOracleReservedCharacters)

isOracleSymbolCharacter :: Char -> Bool
isOracleSymbolCharacter c = any (all ($ c))
  [ [ isAlphaNum ]
  
  , [ ((||) <$> isPunctuation <*> isSymbol)
    , (not . (`elem` "{}"))
    ]
    
  ] --TODO make consistent with Mana cost in the upper right, and the rest of the syntax

----------------------------------------

{-


> [' '..'\DEL']
" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\DEL"


-}


----------------------------------------

-- | Token parser @semiSep p@ parses /zero/ or more occurrences of @p@
-- separated by 'newline'. Returns a list of values returned by @p@.
-- 
lineSep :: TokenParsing m => m a -> m [a]
lineSep p = sepBy p newline
{-# INLINE lineSep #-}

-- -- | Parses a newline character, @\n@.
-- newline :: TokenParsing m => m ()
-- newline = char '\n' $> ()

----------------------------------------

{-|

e.g.

@
*/1+*
*/*+1
@


@
>>> numericTarmogoyf = NumericCreature (Body (SimpleNumeric NumericWildcard) (BinaryNumeric NumericAddition (SimpleNumeric (NumericLiteral 1)) (SimpleNumeric NumericWildcard))) :: Numeric Int
>>> parseNumeric "*/1+*" == numericTarmogoyf
True
>>> parseNumeric "*/*+1" == numericTarmogoyf
True
@

-}
parseNumeric
  :: (Integral i)
  => Parse (Numeric i)
parseNumeric
  = P.parseString pNumeric mempty
  > result2maybe

-- | see 'pLoyalty'
parseLoyalty
  :: (Integral i)
  => Parse (Numeric i)
parseLoyalty
  = P.parseString pLoyalty mempty
  > result2maybe
  > fmap NumericLoyalty

-- -- | see 'pNumericExpression'
-- parseNumericBody
--   :: (Integral i)
--   => Text -> Text -> Maybe (NumericExpression i)
-- parseNumericBody p t = do
--   parseNumericExpression

-- | see 'pNumericExpression'
parseNumericExpression
  :: (Integral i)
  => Parse (NumericExpression i)
parseNumericExpression 
  = P.parseString pNumericExpression mempty
  > result2maybe
  
----------------------------------------

-- | see 'pManaSymbol'
parseManaCost
  :: (Integral i)
  => String
  -> Maybe (ManaCost i)
parseManaCost
  = P.parseString pManaCost mempty  -- runParser
  > result2maybe

----------------------------------------

parseSupertype :: Parse Supertype
parseSupertype = print2parse displaySupertype

parseBaseType :: Parse BaseType
parseBaseType = print2parse displayBaseType

--parseSubtype :: Parse Subtype
--parseSubtype = print2parse displaySubtype
  
----------------------------------------

pNumeric
  :: forall i p. (Integral i)
  => (MonadFail p, TokenParsing p)
  => p (Numeric i)

pNumeric = choice $
  [ try $ NumericCreature <$> pBody
                          <?> "a power/toughness (two expressions)"

  , try $ NumericLoyalty  <$> pLoyalty
                          <?> "a loyalty (one literal)"
  ]

----------------------------------------

pLoyalty
  :: forall i p. (Integral i)
  => (MonadFail p, TokenParsing p)
  => p i
pLoyalty = pNatural --TODO replace with validator (Between's fromIntegral clips, without failing)

pBody
  :: forall i p. (Integral i)
  => (TokenParsing p)
  => p (Body i)
pBody = Body
 <$> (pNumericExpression <* char '/')
 <*>  pNumericExpression

{-|

Commutative operations (like @+@ i.e. 'NumericAddition') don't care about order (by definition). Thus their arguments can be provided in any order, and are normalized to the same 'NumericExpression'. 

-}
pNumericExpression
  :: forall i p. (Integral i)
  => (TokenParsing p)
  => p (NumericExpression i)
pNumericExpression = choice $
  [ try $ do
       l <- pNumericLiteral
       o <- pNumericOperation
       r <- pNumericLiteral
       pure $ BinaryNumeric o l r

  -- [ try $ BinaryNumeric <$> pNumericLiteral
  --                       <*> pNumericOperation
  --                       <*> pNumericLiteral

  , try $ SimpleNumeric <$> pNumericLiteral
  ]

pNumericLiteral
  :: forall i p. (Integral i)
  => (TokenParsing p)
  => p (NumericLiteral i)
pNumericLiteral = choice $
  [ NumericWildcard <$ char '*'
  , NumericConstant <$> pInteger --TODO replace with validator (Between's fromIntegral clips, without failing)
  ]

pNumericOperation
  :: (CharParsing p)
  => p NumericOperation
pNumericOperation = printer displayNumericOperation
  -- parseNumericOperation 

-- parseNumericOperation :: Parse NumericOperation
-- parseNumericOperation = print2parse displayNumericOperation

----------------------------------------

pManaCost
  :: forall i p. (Integral i)
  => (MonadFail p, TokenParsing p)
  => p (ManaCost i)
pManaCost = ManaCost <$> many pManaSymbol

{-|

the parsers use the printers via 'fromInjective',
but also are more lenient w.r.t. ordering. i.e.
they don't just parse the single canonical form
that's printed out, but any permutations too.


@= 'braces' 'pManaSymbol''@


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
  :: forall i p. (Integral i)
  => (MonadFail p, TokenParsing p)
  => p (ManaSymbol i)
pManaSymbol = braces pManaSymbol'  -- between (char '{') (char '}') $


{-| 

-}
pManaSymbol'
  :: forall i p. (Integral i)
  => (MonadFail p, TokenParsing p)
  => p (ManaSymbol i)
pManaSymbol' = choice
    [ try $ HybridSymbol     <$> pGuild
                             <?> "hybrid symbol"
    
    , try $ PhyrexianSymbol  <$> pPhyrexian
                             <?> "phyrexian symbol"

    , try $ MonoHybridSymbol <$> pMonoHybrid
                             <?> "mono-hybrid symbol"

    , try $ GenericSymbol    <$> pNatural
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
    = pColorChar  <* pSlash <* pSymbol
  <|> pSymbol *> pSlash *> pColorChar
 where
 pSlash = skipOptional $ char '/'
 --permute p

----------------------------------------

pGuild :: forall p. (MonadFail p, CharParsing p) => p Guild
pGuild = do
  guild' <- toGuild <$> pColorChar <*> (char '/' *> pColorChar)

  guild <- guild' & maybe pFailure return
  return guild

  where
  pFailure = unexpected expectation --TODO ignored
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

----------------------------------------  

pColorChar :: forall p. (CharParsing p) => p Color
pColorChar = printer displayColorChar

-- pColorWord :: forall p. (CharParsing p) => p Color
-- pColorWord = printer displayColorWord

     -- chars
     -- [ 'W' $> White
     -- , 'U' $> Blue
     -- , 'B' $> Black
     -- , 'R' $> Red
     -- , 'G' $> Green
     -- ]

----------------------------------------

pInteger
  :: forall i p. (Integral i)
  => (TokenParsing p)
  => p i
pInteger = (integer <&> fromIntegral)

pNatural
  :: forall i p. (Integral i)
  => (TokenParsing p)
  => p i
pNatural = (natural <&> fromIntegral)

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

  {-

error $ "Invalid mana cost " <> show s <> ": " <> show err

import qualified Text.ParserCombinators.ReadP as ReadP
import           Text.ParserCombinators.ReadP (ReadP)


-}

