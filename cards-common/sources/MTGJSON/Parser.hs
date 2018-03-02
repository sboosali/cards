{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE DeriveAnyClass, RankNTypes #-}

{-|

-}
module MTGJSON.Parser where

import MTGJSON.Extra hiding (try)
import MTGJSON.Constants
import MTGJSON.Types
import MTGJSON.Known

import MTGJSON.Printer.Finite

import           "parsers"  Text.Parser.Combinators 
import qualified "parsers"  Text.Parser.Char as P
import           "parsers"  Text.Parser.Char (CharParsing)        
import qualified "parsers"  Text.Parser.Token as Token
import           "parsers"  Text.Parser.Token (TokenParsing)
--import "parsers" Text.Parser.Permutation
--import "parsers"  Text.Parser.Token.Style

import qualified "trifecta" Text.Trifecta as Trifecta

import qualified Data.Text.Lazy as T

--import qualified "parsec" Text.Parsec as Parsec

--import qualified Data.List.NonEmpty as NonEmpty

--import Prelude (read)

----------------------------------------

{-

-- > (rC, rT, rL, rU)
-- (Success [["w","x"],["y","z"]],Success [["w","x","y","z"]],Success [["w","x","y","z"]],Success [["w","x","y","z"]])

rs = (rC, rT, rL, rU)

pC :: (TokenParsing p) => p [[String]]  
pC = sepBy1 (many (some (satisfy isAlphaNum) <* skipMany (satisfy $ \c -> c /= '\n' && isSpace c))) newline

rC :: Result [[String]]
rC = trifecta pC "w x \ny z"

pT :: (TokenParsing p) => p [[String]]  
pT = sepBy1 (many (token (some (satisfy isAlphaNum)))) newline

rT :: Result [[String]]
rT = trifecta pT "w x \ny z"

pL :: (TokenParsing p, Monad p) => p [[String]]  
pL = id (sepBy1 (id (many (ident haskellIdents))) newline)

rL :: Result [[String]]
rL = trifecta pL "w x \ny z"

pU :: (TokenParsing p, Monad p) => p [[String]]  
pU = runUnlined (sepBy1 (many (Unlined (ident haskellIdents))) newline)

rU :: Result [[String]]
rU = trifecta pU "w x \ny z"

-}

-- pU :: (TokenParsing p, Monad p) => p [[String]]  
-- pU = lift $ Unlined (sepBy1 (many (ident haskellIdents)) newline)

{-

pU :: (TokenParsing p, Monad p) => Unlined p [[String]]  
pU = Unlined (sepBy1 (many (ident haskellIdents)) newline)

rU :: Result [[String]]
rU = trifectaUnlined pU "w x \ny z"

trifectaUnlined
  :: forall a.
     (forall p. (TokenParsing p, Monad p) => Unlined p a)
  -> String
  -> Result a
-- trifecta :: forall a. (forall p. TokenParsing p => p a) -> String -> Result a
trifectaUnlined p s = P.parseString (runUnlined p) mempty s

-}

----------------------------------------

-- trifectaUnlined
--   :: forall a.
--      (forall p. TokenParsing p => p a)
--   -> String
--   -> Result a
-- trifectaUnlined p = trifecta (Unlined p)

-- trifectaUnlined
--   :: forall a.
--      (forall p. TokenParsing p => p a)
--   -> String
--   -> Result a
-- trifectaUnlined pLined s = r
--   where
--   pUnlined :: Unlined  
--   pUnlined = Unlined pLined
--   r = P.parseString pUnlined mempty s

{-
parsec
  :: forall a m.
     (forall p.
      ( TokenParsing p
      , Monad p
      , Parsec.Stream String m Char
      ) => m p a
     )
  -> String
  -> m (Either Parsec.ParseError a)
parsec = runParserT

{-NOTES

runParserT :: Stream s m t => ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)

-}
-}


{-| simple @trifecta@ parser runner (wraps 'P.parseString').  

-}
trifecta
  :: (forall p.
      ( TokenParsing p
      , MonadFail p
      , MonadPlus p
      -- NOTE required by a callee to satisfy `instance (TokenParsing m, MonadPlus m) => TokenParsing (ReaderT e m)`
      ) => p a
     )
  -> String
  -> Trifecta.Result a
-- trifecta :: forall a. (forall p. TokenParsing p => p a) -> String -> Result a
trifecta p s = Trifecta.parseString p mempty s

{-
trifectaOracle
  :: Name
  -> String
  -> Trifecta.Result Oracle
-- trifecta :: forall a. (forall p. TokenParsing p => p a) -> String -> Result a
trifectaOracle name =
  trifecta (runReaderT pOracle name)
-}

-- trifecta :: Parser a -> String -> Result a
-- trifecta p = Trifecta.parseString p mempty

{-e.g. trifecta: https://github.com/ekmett/trifecta/blob/master/examples/rfc2616/RFC2616.hs


messageHeader :: (Monad m, TokenParsing m) => m Header
messageHeader = (\h b c -> Header h (b : c))
            <$!> (highlight ReservedIdentifier (some token)  <?> "header name")
             <*  highlight Operator (char ':') <* skipHSpaces
             <*> (highlight Identifier (manyTill anyChar endOfLine) <?> "header value")
             <*> (many (skipHSpaces *> manyTill anyChar endOfLine) <?> "blank line")



-}



{-

{-| simple @trifecta@ parser runner (wraps 'Trifecta.parseString').

Wraps the given parser @p@ in 'Unlined', used by 'pOracle', since 'OracleParagraph's are split on newlines. 


-}
trifectaUnlined
  :: forall p a.
    ( TokenParsing p
    )
  => p a -> String -> Result a
trifectaUnlined pLined = Trifecta.parseString pUnlined mempty
  where
  pUnlined :: Unlined p a
  pUnlined = Unlined pLined

-- {-| simple @trifecta@ parser runner (wraps 'Trifecta.parseString').

-- Wraps the given parser @p@ in 'Unlined', used by 'pOracle', since 'OracleParagraph's are split on newlines. 

-- -}
-- trifectaUnlined
--   :: forall p a.
--     ( TokenParsing p
--     )
--   => p a -> String -> Result a
-- trifectaUnlined pLined = Trifecta.parseString pUnlined mempty
--   where
--   pUnlined = Unlined pLined

-}

----------------------------------------
-- Sets

parseEditionType :: Parse EditionType
parseEditionType = print2parse displayEditionType

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

see 'displayLayout'

-}
parseLayout :: Parse Layout
parseLayout = print2parse displayLayout

----------------------------------------

{-|

@
>>> let _BallynockTrapper = "{T}: Tap target creature.\nWhenever you cast a white spell, you may untap "
>>> parseOracle nameless (_BallynockTrapper <> "~") == parseOracle "Ballynock Trapper" (_BallynockTrapper <> "Ballynock Trapper")
True
@

@
>>> parseOracle "{T}: Tap target creature.\nWhenever you cast a white spell, you may untap ~."
Just (OracleFrames (OracleFrame [OracleParagraph [OracleSymbol (Right (MiscellaneousSymbol TapSymbol)),OraclePhrase [":","Tap","target","creature."]],OracleParagraph [OraclePhrase ["Whenever","you","cast","a","white","spell,","you","may","untap"],OracleNamesake,OraclePhrase ["."]]] :| []))
>>> parseOracle "Ballynock Trapper" "{T}: Tap target creature.\nWhenever you cast a white spell, you may untap Ballynock Trapper."
@


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

>>> parseOracle "{T}: Tap target creature.\nWhenever you cast a white spell, you may untap ~." == oracleBallynockTrapper
True


e.g. @'vanilla'@

>>> :set -XOverloadedLists
>>> [] == vanilla
True
>>> :set -XOverloadedStrings
>>> "" == vanilla
True

-}
parseOracle :: Knicknames -> TParse Oracle
parseOracle knicknames
  = parseOracleResult knicknames
  > result2maybe

{-|

see 'parseOracle'

-}
parseOracleLoosely :: Knicknames -> Text -> Oracle
parseOracleLoosely knicknames s
  = s
  & parseOracle knicknames
  & fromMaybe (OracleVerbatim s)

{-| see 'parseOracle'. 

-}
parseOracleResult :: Knicknames -> Text -> Trifecta.Result Oracle
parseOracleResult knicknames
  = mungeOracleText knicknames
  > T.unpack
 -- > trifecta pOracle
  > trifecta (runReaderT' knicknames pOracle)

{-| @= 'parseOracle' 'knicknameless'@

-}
parseOracle_ :: TParse Oracle
parseOracle_ = parseOracle knicknameless

{-|

see 'parseOracle_'

-}
parseOracleResult_ :: Text -> Trifecta.Result Oracle
parseOracleResult_ = parseOracleResult knicknameless

{-|

see 'parseOracle_'

-}
parseOracleLoosely_ :: Text -> Oracle
parseOracleLoosely_ = parseOracleLoosely knicknameless

----------------------------------------

{- |

Replaces newlines with 'puaNewline's, a Private Use Area character (a hack because I can't get 'Unlined' to work).


e.g. @Phage the Untouchable@ is abbreviated:

@
When Phage the Untouchable enters the battlefield, if you didn't cast it from your hand, you lose the game.
Whenever Phage deals combat damage to a creature, destroy that creature. It can't be regenerated.
Whenever Phage deals combat damage to a player, that player loses the game.
@



-}
mungeOracleText :: Knicknames -> Text -> Text
mungeOracleText (knicknames2text -> names)
  = replaceNewlines
  > replaceKnicknames

  where
  replaceNewlines = T.replace "\n" tPseudoNewline
  
  replaceKnicknames = foldr (.) id (replaceName <$> names)
  replaceName name = T.replace name "~" 

-- mungeOracleText (Knicknames (Name name :| _)) --TODO
--   = T.replace "\n" tPseudoNewline
--   > T.replace name "~" 

{-

  = Trifecta.parseString p mempty
  > fmap runUnlined
  > result2maybe
  where
  p = Unlined pOracle 

> parseOracle "{T}: Tap target creature.\nWhenever you cast a white spell, you may untap ~."
Just (OracleFrames (OracleFrame [OracleParagraph [OracleSymbol (Right (MiscellaneousSymbol TapSymbol))]] :| []))

trifecta pOracleParagraph "{U/G}{?}: Tap or untap ~.\nHexproof"


-}

----------------------------------------



{-|

e.g. @Student Of Warfare@: 

@
      {
        "artist": "Volkan Baga",
        "cmc": 1,
        "colorIdentity": [
          "W"
        ],
        "colors": [
          "White"
        ],
        "foreignNames": [
          {
            "language": "French",
            "name": "Étudiante de la guerre",
            "multiverseid": 215684
          },
          {
            "language": "Russian",
            "name": "Ученица Войны",
            "multiverseid": 215932
          },
          {
            "language": "Chinese Simplified",
            "name": "战场修练者",
            "multiverseid": 216180
          },
          {
            "language": "Italian",
            "name": "Studentessa della Guerra",
            "multiverseid": 216428
          },
          {
            "language": "Japanese",
            "name": "闘争の学び手",
            "multiverseid": 216676
          },
          {
            "language": "Portuguese (Brazil)",
            "name": "Estudante de Guerra",
            "multiverseid": 216924
          },
          {
            "language": "Spanish",
            "name": "Estudiante de la guerra",
            "multiverseid": 217172
          },
          {
            "language": "German",
            "name": "Student der Kriegskunst",
            "multiverseid": 217420
          }
        ],
        "id": "bc1027406aafcdc50fa60fbfec02184a65b51a71",
        "imageName": "student of warfare",
        "layout": "leveler",
        "legalities": [
          {
            "format": "Commander",
            "legality": "Legal"
          },
          {
            "format": "Legacy",
            "legality": "Legal"
          },
          {
            "format": "Modern",
            "legality": "Legal"
          },
          {
            "format": "Vintage",
            "legality": "Legal"
          },
          {
            "format": "Zendikar Block",
            "legality": "Legal"
          }
        ],
        "manaCost": "{W}",
        "mciNumber": "47",
        "multiverseid": 193598,
        "name": "Student of Warfare",
        "number": "47",
        "originalText": "Level up {W} ({W}: Put a level counter on this. Level up only as a sorcery.)\nLEVEL 2-6\n3/3\nFirst strike\nLEVEL 7+\n4/4\nDouble strike",
        "originalType": "Creature — Human Knight",
        "power": "1",
        "printings": [
          "ROE"
        ],
        "rarity": "Rare",
        "rulings": [
          {
            "date": "2010-06-15",
            "text": "The abilities a leveler grants to itself don't overwrite any other abilities it may have. In particular, they don't overwrite the creature's level up ability; it always has that."
          },
          {
            "date": "2010-06-15",
            "text": "Effects that set a leveler's power or toughness to a specific value, including the effects from a level symbol's ability, apply in timestamp order. The timestamp of each level symbol's ability is the same as the timestamp of the leveler itself, regardless of when the most recent level counter was put on it."
          },
          {
            "date": "2010-06-15",
            "text": "Effects that modify a leveler's power or toughness, such as the effects of Giant Growth or Glorious Anthem, will apply to it no matter when they started to take effect. The same is true for counters that change the creature's power or toughness (such as +1/+1 counters) and effects that switch its power and toughness."
          },
          {
            "date": "2010-06-15",
            "text": "If another creature becomes a copy of a leveler, all of the leveler's printed abilities — including those represented by level symbols — are copied. The current characteristics of the leveler, and the number of level counters on it, are not. The abilities, power, and toughness of the copy will be determined based on how many level counters are on the copy."
          },
          {
            "date": "2010-06-15",
            "text": "A creature's level is based on how many level counters it has on it, not how many times its level up ability has been activated or has resolved. If a leveler gets level counters due to some other effect (such as Clockspinning) or loses level counters for some reason (such as Vampire Hexmage), its level is changed accordingly."
          }
        ],
        "subtypes": [
          "Human",
          "Knight"
        ],
        "text": "Level up {W} ({W}: Put a level counter on this. Level up only as a sorcery.)\nLEVEL 2-6\n3/3\nFirst strike\nLEVEL 7+\n4/4\nDouble strike",
        "toughness": "1",
        "type": "Creature — Human Knight",
        "types": [
          "Creature"
        ]
      },
@

pOracleLeveler
  :: forall p.
    ( TokenParsing p
    )
  => p Oracle
pOracleLeveler = _
-}

{-|


@
 Trifecta.parseString pOracle mempty "{T}: Tap target creature.\nWhenever you cast a white spell, you may untap ~."
@


-}
pOracle
  :: forall p.
    ( TokenParsing p
    , MonadFail p
    , MonadReader Knicknames p
    )
  => p Oracle
pOracle = pOracleFrame
  <&> (:|[]) <&> OracleFrames --TODO levelers
{-
-}

{-|

e.g.

@
{
  "name": "Student of Warfare",
  "text": "Level up {W} ({W}: Put a level counter on this. Level up only as a sorcery.)\nLEVEL 2-6\n3/3\nFirst strike\nLEVEL 7+\n4/4\nDouble strike"
}
@

a.k.a., which is parsed into:

@
{
  "name": "Student of Warfare",
  "text": {
            "frame": "leveler",
            "cost": "Level up {W} ({W}: Put a level counter on this. Level up only as a sorcery.)",
            "levels": [
                        {
                          "range": "2-6",
                          "pt": "3/3",
                          "text": "First strike"
                        },
                        {
                          "range": "7+",
                          "pt": "4/4",
                          "text":"Double strike"
                        }
                      ]
           }
}
@

or?

@
{
  "name": "Student of Warfare",
  "text": {
            "frame": "leveler",
            "levels": [
                        {
                          "range": "",
                          "pt": "",
                          "text": "Level up {W} ({W}: Put a level counter on this. Level up only as a sorcery.)",
                        },
                        {
                          "range": "2-6",
                          "pt": "3/3",
                          "text": "First strike"
                        },
                        {
                          "range": "7+",
                          "pt": "4/4",
                          "text":"Double strike"
                        }
                      ]
          }
}
@

-}
pOracleFrame
  :: forall p.
    ( TokenParsing p
    , MonadFail p
    , MonadReader Knicknames p
    )
  => p OracleFrame
pOracleFrame = pseudoLineSep pOracleParagraph
  <&> OracleFrame


{-
pOracleFrame = lineSep pOracleParagraph
  <&> OracleFrame
-}

  {-NOTE the `Unlined` parser transformaer temporarily (?) disables the  automatic trailing newline (but not whitespace-in-general) consumption of the given token parser.
  -}

-- pOracleFrame = runUnlined $ lineSep (Unlined pOracleParagraph)

{-|


-}
pOracleParagraph
  :: forall p.
    ( TokenParsing p
    , MonadFail p
    , MonadReader Knicknames p
    )
  => p OracleParagraph
  
-- pOracleParagraph = pRelinedChunks
--   <&> OracleParagraph
--   where
--   pRelinedChunks = runUnlined $ many pUnlinedChunk
--   pUnlinedChunk = Unlined pOracleChunk

pOracleParagraph = many pOracleChunk
  <&> OracleParagraph

{-|


-}
pOracleChunk
  :: forall p.
    ( TokenParsing p
    , MonadFail p
    , MonadReader Knicknames p
    )
  => p OracleChunk
pOracleChunk = choice
  [ try $ OracleNamesake <$
            pOracleNamesakeStatic -- pOracleNamesakeDynamic
    -- one of a finite vocab
  , try $ OracleSymbol   <$> pOracleSymbol
    -- has a "{" prefix
  , try $ OraclePhrase   <$> pOraclePhrase
    -- anything non-reserved
  ]

----------------------------------------

{-|

>>> trifecta pOracleParagraph "{U/G}{?}: Tap or untap ~."
Success (OracleParagraph [OracleSymbol (Right (ManaSymbol (HybridSymbol Simic))),OracleSymbol (Left "?"),OraclePhrase ":",OraclePhrase ["Tap","or","untap"],OracleNamesake,OraclePhrase ["."]])

-}
pOraclePhrase
  :: forall p.
    ( TokenParsing p
    )
  => p [Text]
pOraclePhrase = some pOracleWord

{-|


-}
pOracleWord
  :: forall p.
    ( TokenParsing p
    )
  => p Text
pOracleWord = pW <&> fromString -- T.pack
 where
 pW = token (some pC)
 pC = P.satisfy isOracleTokenChar

{-NOTE

class CharParsing m => TokenParsing m where
  someSpace :: m ()
  someSpace = skipSome (satisfy isSpace)
  token :: m a -> m a
  token p = p <* (someSpace <|> pure ())

instance TokenParsing m => TokenParsing (Unlined m) where
  someSpace = skipMany (satisfy $ \c -> c /= '\n' && isSpace c)

instance TokenParsing m => TokenParsing (Unspaced m) where
  someSpace = empty


-}

----------------------------------------

{-|

e.g. @Squadron Hawk@

@
{
        "name": "Squadron Hawk",
        "text": "Flying\nWhen Squadron Hawk enters the battlefield, you may search your library for up to three cards named Squadron Hawk, reveal them, put them into your hand, then shuffle your library.",
        ...
}
@



TODO
@o:"named ~"@ versus @o:"named" (not o:"named ~")@



-}
pOracleNamesakeDynamic
  :: forall p.
    ( TokenParsing p
    , MonadReader Knicknames p
    )
  => p ()
pOracleNamesakeDynamic = do
  knicks <- ask
  
  let dynamicNames = knicknames2text knicks <&> toS
  let pDynamicNames = dynamicNames <&> P.text

  let pAllNames
         = (pDynamicNames <&> try)
        ++ pStaticNames

  () <$ choice pAllNames

  where
  pStaticNames = staticNames <&> P.text
  staticNames =
    [ "~"      
    , "CARDNAME"
    ]

  -- let Name name = nameless
  -- let pName = () <$ P.text (name & toSL) 
  
  -- choice
  --     [ pOracleNamesakeStatic
  --     , pName
  --     ]


pOracleNamesakeStatic
  :: forall p.
    ( TokenParsing p
    )
  => p ()
pOracleNamesakeStatic = symbols_
      [ "~"      
      , "CARDNAME"
      ]


-- pOracleNamesake = symbols
--   [ "~"      -: ()
--   , "CARDNAME"-: ()
--   ]

-- pOracleNamesake = () <$ go
--  where
--  go = choice
--      [ string "~"  -- char '~'
--      , string "CARDNAME"
--      ]

----------------------------------------

{-|


-}
pOracleSymbol
  :: forall p.
    ( TokenParsing p
    , MonadFail p
    )
  => p OracleSymbol
pOracleSymbol = Token.braces pOracleSymbol'

  -- = (betweenChars&uncurry) theOracleSymbolWrappingCharacters $
  --     pOracleSymbol'

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
  = f <$> some1 (P.satisfy isOracleSymbolCharacter)
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
  [ try $ ManaSymbol <$> pKnownManaSymbol' --TODO doesn't pManaSymbol `try` enough?
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
pMiscellaneousSymbol' = symbolics
 [ 'T' -: TapSymbol  
 , 'Q' -: UntapSymbol
 ]
 
-- pMiscellaneousSymbol' = choice
--  [ TapSymbol   <$ symbol "T" -- string "T"
--  , UntapSymbol <$ symbol "Q" --string "Q"
--  ]

----------------------------------------

isOracleTokenChar :: Char -> Bool
isOracleTokenChar = predicateDisjunctionOfConjunctions
  --NOTE a disjunction of conjunctions,
  -- i.e. any of the groups,
  -- all of the predicates within a group. 
  [ [ isAlphaNum
    ]
  , [ isntPseudoSpace
    , (/='~') --TODO `CARDNAME`
    , (/='{')
    ]
  ]

isntPseudoSpace :: Char -> Bool
isntPseudoSpace = isPseudoSpace > not

isPseudoSpace :: Char -> Bool
isPseudoSpace = predicateDisjunctionOfConjunctions
  [ [ (== cPseudoNewline)
    ]
  , [ (/= '\n')
    , isSpace
    ]
  ]

{-
isOracleTokenChar = not . isSpace

isOracleTokenChar c = any (all ($ c))
  [ [ isAlphaNum
    ]
  , [ not . isSpace
    -- , not . (=='{')
    ]
  ]
-}

isOracleReservedKeyword :: String -> Bool
isOracleReservedKeyword = (`elem` theOracleReservedKeywords)

isOracleReservedCharacter :: Char -> Bool
isOracleReservedCharacter = (`elem` theOracleReservedCharacters)

isOracleSymbolCharacter :: Char -> Bool
isOracleSymbolCharacter c = any (all ($ c))
  [ [ isAlphaNum ]
  
  , [ ((||) <$> isPunctuation <*> isSymbol)
    , (not . (`elem` theOracleSymbolWrappers))
    ]
    
  ] --TODO make consistent with Mana cost in the upper right, and the rest of the syntax

----------------------------------------
{-


> [' '..'\DEL']
" !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\DEL"


-}

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
  = Trifecta.parseString pNumeric mempty
  > result2maybe

-- | see 'pLoyalty'
parseLoyalty
  :: (Integral i)
  => Parse (Numeric i)
parseLoyalty
  = Trifecta.parseString pLoyalty mempty
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
  = Trifecta.parseString pNumericExpression mempty
  > result2maybe
  
----------------------------------------

-- | see 'pManaSymbol'
parseManaCost
  :: (Integral i)
  => String
  -> Maybe (ManaCost i)
parseManaCost
  = Trifecta.parseString pManaCost mempty  -- runParser
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
 <$> (pNumericExpression <* P.char '/')
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
  [ NumericWildcard <$ P.char '*'
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
pManaSymbol = Token.braces pManaSymbol'  -- between (char '{') (char '}') $


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
    
    ,       VariableSymbol   <$  P.oneOf "XYZ"
                             <?> "variable symbol"
    ]

----------------------------------------

pPhyrexian :: forall p. (CharParsing p) => p Color
pPhyrexian = pPermutedColor $ P.string "P"

pMonoHybrid :: forall p. (CharParsing p) => p Color
pMonoHybrid = pPermutedColor $ P.string "2"

pPermutedColor :: forall p. (CharParsing p) => p String -> p Color
pPermutedColor pSymbol
    = pColorChar  <* pSlash <* pSymbol
  <|> pSymbol *> pSlash *> pColorChar
 where
 pSlash = skipOptional $ P.char '/'
 --permute p

----------------------------------------

pGuild :: forall p. (MonadFail p, CharParsing p) => p Guild
pGuild = do
  guild' <- toGuild <$> pColorChar <*> (P.char '/' *> pColorChar)

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

  

----------------------------------------

pInteger
  :: forall i p. (Integral i)
  => (TokenParsing p)
  => p i
pInteger = (Token.integer <&> fromIntegral)

pNatural
  :: forall i p. (Integral i)
  => (TokenParsing p)
  => p i
pNatural = (Token.natural <&> fromIntegral)

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



pOracleWord
  :: forall p.
    ( TokenParsing p
    )
  => p Text
pOracleWord = p <&> fromString -- T.pack
 where
 p    = good `manyTill` bad
 good = satisfy (not . isSpace)


-}



{-NOTES

notFollowedBy :: Show a => m a -> m () 

notFollowedBy p only succeeds when parser p fails. This parser does not consume any input. This parser can be used to implement the 'longest match' rule. For example, when recognizing keywords (for example let), we want to make sure that a keyword is not followed by a legal identifier character, in which case the keyword is actually an identifier (for example lets). We can program this behaviour as follows:

 keywordLet  = try $ string "let" <* notFollowedBy alphaNum


-}
