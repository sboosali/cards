{-# LANGUAGE OverloadedStrings #-}

{-|

the inverse/opposite of @"MTGJSON.Parser"@. 

-}
module MTGJSON.Printer.Finite where

import MTGJSON.Extra
import MTGJSON.Types

import qualified Data.Text.Lazy as T

import Prelude.Spiros hiding (P)

----------------------------------------

displayNumeric :: (Show i) => Print (Numeric i)
displayNumeric = \case
  Constant i -> displayNumericConstant i
  Variable i -> displayNumericVariable i

displayNumericConstant :: (Show i) => Print (NumericConstant i)
displayNumericConstant = \case
  NumericLiteral i -> show' i
  WildcardConstant -> "*"

displayNumericVariable :: Print NumericVariable
displayNumericVariable = \case
  PowerVariable     -> "pow"
  ToughnessVariable -> "tou"
  CostVariable      -> "cmc"

----------------------------------------

displayChroma :: Print Chroma
displayChroma = \case
 Hue          hue -> displayHue hue
 Multicolored     -> "m"
 LandColor        -> "l"

displayHue :: Print Hue
displayHue = hue2text

displayColor :: Print Color
displayColor = color2text 

displayColorIdentity :: Print ColorIdentity
displayColorIdentity (ColorIdentity hue) = hue2text hue

displayColorIndication :: Print ColorIndication
displayColorIndication (ColorIndication color) = color2text color 

----------------------------------------

displayManaSymbol :: (Show i) => Print (ManaSymbol i)
displayManaSymbol = \case
  GenericSymbol      i         -> displayGenericSymbol   i 
  HueSymbol          hue       -> displayHueSymbol       hue
  HybridSymbol       hybrid    -> displayHybridSymbol    hybrid
  PhyrexianSymbol    phyrexian -> displayPhyrexianSymbol phyrexian

displayGenericSymbol :: (Show i) => Print i
displayGenericSymbol = show > toS > braces

displayHueSymbol :: Print Hue
displayHueSymbol = hue2letter > char2text > braces

-- \case
--   TrueColor c -> displayColor c
--   Colorless c -> "{C}"

-- displayColor :: Print Color
-- displayColor = color2letter > braces

displayHybridSymbol :: (Show i) => Print (Hybrid i)
displayHybridSymbol = \case
  GuildHybrid  guild -> displayGuildHybridSymbol guild
  GrayHybrid i color -> displayGrayHybridSymbol i color

displayGuildHybridSymbol :: Print Guild
displayGuildHybridSymbol
  = fromGuild'
  > fmap (color2text) -- e.g. ["U","G"]
  > T.intercalate "/" -- e.g. "U/G"
  > braces            -- e.g. "{U/G}"
  --displayColorHybrid colors = (colors & fromGuild) & 

displayGrayHybridSymbol :: (Show i) => i -> Color -> Text
displayGrayHybridSymbol i c = displayRawHybridSymbol i' c'
  where
  i' = show' i
  c' = char2text (color2letter c)

displayRawHybridSymbol :: Text -> Text -> Text
displayRawHybridSymbol x y = "{" <> t <> "}"
  where
  t = x <> "/" <> y -- [x,y]

displayPhyrexianSymbol :: Print Phyrexian
displayPhyrexianSymbol = \case
  Phyrexian c -> "{P" <> t <> "}"
                     where
                     t = char2text (color2letter c)

---------------------------------------

-- | in ascending (canonical, i.e. WUBRG) order
fromGuild' :: Guild -> [Color]
fromGuild' = fromGuild > pair2list > sort
 where
 pair2list (x,y) = [x,y]

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

fromGuild :: Guild -> (Color,Color) --TODO unordered tuple?
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

fromSlice :: Slice -> (Color,Color,Color)
fromSlice = \case
  Shard x -> fromShard x
  Wedge x -> fromWedge x

fromShard :: Shard -> (Color,Color,Color)
fromShard = \case
 Bant   -> (Green,Wedge,Blue)
 Esper  -> (Wedge,Blue,Black)
 Grixis -> (Blue,Black,Red)
 Jund   -> (Black,Red,Green)
 Naya   -> (Red,Green,White)

fromWedge :: Wedge -> (Color,Color,Color)
fromWedge = \case 
  Abzan  -> (White,Black,Green)
  Jeskai -> (Blue,Red,White)
  Sultai -> (Black,Green,Blue)
  Mardu  -> (Red,White,Black)
  Temur  -> (Green,Blue,Red)

fromNephilim :: Nephilim -> (Color,Color,Color,Color)
fromNephilim = \case 
 Artifice   -> (White, Blue, Black, Red)
 Chaos      -> (Blue, Black, Red, Green)
 Aggression -> (Black, Red, Green, White)
 Altruism   -> (Red, Green, White, Blue)
 Growth     -> (Growth, White, Blue, Black)
                                                                                                             
---------------------------------------

displayLayout :: Layout -> Text
displayLayout = \case
 Aftermath   -> "aftermath"
 DoubleFaced -> "double-faced"
 Flip        -> "flip"
 Leveler     -> "leveler"
 Meld        -> "meld"
 Normal      -> "normal"
 Phenomenon  -> "phenomenon"
 Plane       -> "plane"
 Scheme      -> "scheme"
 Split       -> "split"
 Token       -> "token"
 Vanguard    -> "vanguard"  

---------------------------------------

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

{-|

@
= 'language2abbreviation'
@

>>> displayLanguage English
"en"

-}
displayLanguage :: Print Language
displayLanguage = language2abbreviation
  
----------------------------------------

languageInfo :: Language -> LanguageInfo
languageInfo = \case  
 English    -> LanguageInfo "en" "English"
 German     -> LanguageInfo "de" "Deutsch"
 French     -> LanguageInfo "fr" "Français"
 Italian    -> LanguageInfo "it" "Italiano"
 Spanish    -> LanguageInfo "es" "Español"
 Portuguese -> LanguageInfo "pt" "Português"
 Japanese   -> LanguageInfo "jp" "日本語"
 Chinese    -> LanguageInfo "cn" "简体中文"
 Russian    -> LanguageInfo "ru" "Русский"
 Taiwanese  -> LanguageInfo "tw" "繁體中文"
 Korean     -> LanguageInfo "ko" "한국어"

language2abbreviation :: Language -> Text
language2abbreviation = languageInfo > _languageAbbreviation

language2endonym :: Language -> Text
language2endonym = languageInfo > _languageEndonym

----------------------------------------

{-|

see 'edition2abbreviation' and 'language2abbreviation'. 

>>> displayQualifiedEdition $ QualifiedEdition Alpha Nothing
"al"

>>> displayQualifiedEdition $ QualifiedEdition Alpha (Just English)
"al/en"

-}
displayQualifiedEdition :: Print QualifiedEdition
displayQualifiedEdition QualifiedEdition{..} = e <> l
  where
  e  = _qEdition  & edition2abbreviation
  l  = l' & maybe "" (\t -> "/" <> t)
  l' = _qLanguage <&> displayLanguage 

----------------------------------------

edition2block :: Edition -> Block
edition2block = editionInfo > _editionBlock 

edition2abbreviation :: Edition -> Text
edition2abbreviation = editionInfo > _editionAbbreviation

edition2description :: Edition -> Text
edition2description = editionInfo > _editionDescription

editionInfo :: Edition -> EditionInfo
editionInfo = \case

 RIX    -> EditionInfo Ixalan               "rix" "Rivals of Ixalan"
 XLN    -> EditionInfo Ixalan               "xln" "Ixalan"

 HOU    -> EditionInfo Amonkhet             "hou" "Hour of Devastation" 
 AKH    -> EditionInfo Amonkhet             "akh" "Amonkhet" 

 AER    -> EditionInfo Kaladesh             "aer" "Aether Revolt"
 KLD    -> EditionInfo Kaladesh             "kld" "Kaladesh"

 EMN    -> EditionInfo ShadowsOverInnistrad "emn" "Eldritch Moon"
 SOI    -> EditionInfo ShadowsOverInnistrad "soi" "Shadows over Innistrad"

 OGW    -> EditionInfo BattleForZendikar    "ogw" "Oath of the Gatewatch"
 BFZ    -> EditionInfo BattleForZendikar    "bfz" "Battle for Zendikar"

 DTK    -> EditionInfo KhansOfTarkir        "dtk" "Dragons of Tarkir"
 FRF    -> EditionInfo KhansOfTarkir        "frf" "Fate Reforged"
 KTK    -> EditionInfo KhansOfTarkir        "ktk" "Khans of Tarkir"

 JOU    -> EditionInfo Theros               "jou" "Journey into Nyx"
 BNG    -> EditionInfo Theros               "bng" "Born of the Gods"
 THS    -> EditionInfo Theros               "ths" "Theros"

 DGM    -> EditionInfo ReturnToRavnica      "dgm" "Dragon's Maze"
 GTC    -> EditionInfo ReturnToRavnica      "gtc" "Gatecrash"
 RTR    -> EditionInfo ReturnToRavnica      "rtr" "Return to Ravnica"

 AVR    -> EditionInfo InnistradCycle       "avr" "Avacyn Restored"
 DKA    -> EditionInfo InnistradCycle       "dka" "Dark Ascension"
 ISD    -> EditionInfo InnistradCycle       "isd" "Innistrad"

 NPH    -> EditionInfo ScarsOfMirrodin      "nph" "New Phyrexia"
 MBS    -> EditionInfo ScarsOfMirrodin      "mbs" "Mirrodin Besieged"
 SOM    -> EditionInfo ScarsOfMirrodin      "som" "Scars of Mirrodin"

 ROE    -> EditionInfo ZendikarCycle        "roe" "Rise of the Eldrazi"
 WWK    -> EditionInfo ZendikarCycle        "wwk" "Worldwake"
 ZEN    -> EditionInfo ZendikarCycle        "zen" "Zendikar"

 ARB    -> EditionInfo ShardsOfAlara        "arb" "Alara Reborn"
 CFX    -> EditionInfo ShardsOfAlara        "cfx" "Conflux"
 ALA    -> EditionInfo ShardsOfAlara        "ala" "Shards of Alara"

 EVE    -> EditionInfo ShadowmoorCycle      "eve" "Eventide"
 SHM    -> EditionInfo ShadowmoorCycle      "shm" "Shadowmoor"

 MT     -> EditionInfo LorwynCycle          "mt" "Morningtide"
 LW     -> EditionInfo LorwynCycle          "lw" "Lorwyn"

 FUT    -> EditionInfo TimeSpiralCycle      "fut" "Future Sight"
 PC     -> EditionInfo TimeSpiralCycle      "pc" "Planar Chaos"
 TS     -> EditionInfo TimeSpiralCycle      "ts" "Time Spiral"
 TSTS   -> EditionInfo TimeSpiralCycle      "tsts" "Time Spiral Timeshifted"

 CS     -> EditionInfo IceAgeCycle          "cs" "Coldsnap"
 AI     -> EditionInfo IceAgeCycle          "ai" "Alliances"
 IA     -> EditionInfo IceAgeCycle          "ia" "Ice Age"

 DI     -> EditionInfo RavnicaCycle         "di" "Dissension"
 GP     -> EditionInfo RavnicaCycle         "gp" "Guildpact"
 RAV    -> EditionInfo RavnicaCycle         "rav" "Ravnica: City of Guilds"

 SOK    -> EditionInfo KamigawaCycle        "sok" "Saviors of Kamigawa"
 BOK    -> EditionInfo KamigawaCycle        "bok" "Betrayers of Kamigawa"
 CHK    -> EditionInfo KamigawaCycle        "chk" "Champions of Kamigawa"

 DN5    -> EditionInfo MirrodinCycle        "5dn" "Fifth Dawn"
 DS     -> EditionInfo MirrodinCycle        "ds" "Darksteel"
 MI     -> EditionInfo MirrodinCycle        "mi" "Mirrodin"

 SC     -> EditionInfo OnslaughtCycle       "sc" "Scourge"
 LE     -> EditionInfo OnslaughtCycle       "le" "Legions"
 ON     -> EditionInfo OnslaughtCycle       "on" "Onslaught"

 JU     -> EditionInfo OdysseyCycle         "ju" "Judgment"
 TR     -> EditionInfo OdysseyCycle         "tr" "Torment"
 OD     -> EditionInfo OdysseyCycle         "od" "Odyssey"

 AP     -> EditionInfo InvasionCycle        "ap" "Apocalypse"
 PS     -> EditionInfo InvasionCycle        "ps" "Planeshift"
 IN     -> EditionInfo InvasionCycle        "in" "Invasion"

 PR     -> EditionInfo MasqueradeCycle      "pr" "Prophecy"
 NE     -> EditionInfo MasqueradeCycle      "ne" "Nemesis"
 MM     -> EditionInfo MasqueradeCycle      "mm" "Mercadian Masques"

 UD     -> EditionInfo ArtifactsCycle       "ud" "Urza's Destiny"
 UL     -> EditionInfo ArtifactsCycle       "ul" "Urza's Legacy"
 US     -> EditionInfo ArtifactsCycle       "us" "Urza's Saga"

 EX     -> EditionInfo RathCycle            "ex" "Exodus"
 SH     -> EditionInfo RathCycle            "sh" "Stronghold"
 TP     -> EditionInfo RathCycle            "tp" "Tempest"

 WL     -> EditionInfo MirageCycle          "wl" "Weatherlight"
 VI     -> EditionInfo MirageCycle          "vi" "Visions"
 MR     -> EditionInfo MirageCycle          "mr" "Mirage"

 HL     -> EditionInfo EarlySets            "hl" "Homelands"
 FE     -> EditionInfo EarlySets            "fe" "Fallen Empires"
 DK     -> EditionInfo EarlySets            "dk" "The Dark"
 LG     -> EditionInfo EarlySets            "lg" "Legends"
 AQ     -> EditionInfo EarlySets            "aq" "Antiquities"
 AN     -> EditionInfo EarlySets            "an" "Arabian Nights"

 ORI    -> EditionInfo CardinalCore         "ori" "Magic Origins"
 M15    -> EditionInfo CardinalCore         "m15" "Magic 2015"
 M14    -> EditionInfo CardinalCore         "m14" "Magic 2014 Core Set"
 M13    -> EditionInfo CardinalCore         "m13" "Magic 2013"
 M12    -> EditionInfo CardinalCore         "m12" "Magic 2012"
 M11    -> EditionInfo CardinalCore         "m11" "Magic 2011"
 M10    -> EditionInfo CardinalCore         "m10" "Magic 2010"

 E10    -> EditionInfo OrdinalCore          "10e" "Tenth Edition"
 E9     -> EditionInfo OrdinalCore          "9e" "Ninth Edition"
 E8     -> EditionInfo OrdinalCore          "8e" "Eighth Edition"
 E7     -> EditionInfo OrdinalCore          "7e" "Seventh Edition"
 E6     -> EditionInfo OrdinalCore          "6e" "Classic Sixth Edition"
 E5     -> EditionInfo OrdinalCore          "5e" "Fifth Edition"
 E4     -> EditionInfo OrdinalCore          "4e" "Fourth Edition"

 SUMMER -> EditionInfo Antediluvian         "summer" "Revised Edition Edgar"
 RV     -> EditionInfo Antediluvian         "rv" "Revised Edition"
 UN     -> EditionInfo Antediluvian         "un" "Unlimited Edition"
 BE     -> EditionInfo Antediluvian         "be" "Limited Edition Beta"
 AL     -> EditionInfo Antediluvian         "al" "Limited Alpha Edition"

----------------------------------------
