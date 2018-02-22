--{-# LANGUAGE OverloadedLabels, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-|

the inverse/opposite of @"Cards.Syntax.MagicCardsInfo.Parser"@. 

-}
module Cards.Syntax.MagicCardsInfo.Printer where

import Cards.Syntax.Extra
import Cards.Syntax.MagicCardsInfo.Types

import qualified Data.Text.Lazy as T

import Prelude.Spiros hiding (P)

----------------------------------------


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

-- languageAbbreviation :: Language -> Text
-- languageAbbreviation = languageInfo > _abbreviation

-- languageEndonym :: Language -> Text
-- languageEndonym = languageInfo > _endonym

----------------------------------------

-- editionBlock :: Edition -> Block
-- editionBlock = editionInfo > _block 

-- editionAbbreviation :: Edition -> Text
-- editionAbbreviation = editionInfo > _abbreviation

-- editionDescription :: Edition -> Text
-- editionDescription = editionInfo > _description

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
