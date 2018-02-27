{-# LANGUAGE OverloadedStrings #-}

{-|

the inverse/opposite of @"MTGJSON.Parser"@. 

-}
module MTGJSON.Printer.Finite where

import MTGJSON.Extra
--import MTGJSON.Types
import MTGJSON.Known

----------------------------------------

{-

numericTarmogoyf = NumericCreature (Body (SimpleNumeric NumericWildcard) (BinaryNumeric NumericAddition (SimpleNumeric (NumericLiteral 1)) (SimpleNumeric NumericWildcard)))
displayNumeric numericTarmogoyf

e.g.

@
> numericTarmogoyf = 'NumericCreature' ('Body' ('SimpleNumeric' 'NumericWildcard') ('BinaryNumeric' 'NumericAddition' ('SimpleNumeric' ('NumericLiteral' 1)) ('SimpleNumeric' 'NumericWildcard'))) :: 'Numeric' Int
@

>>> numericTarmogoyf = NumericCreature (Body (SimpleNumeric NumericWildcard) (BinaryNumeric NumericAddition (SimpleNumeric (NumericLiteral 1)) (SimpleNumeric NumericWildcard))) :: Numeric Int
>>> displayNumeric numericTarmogoyf
"*/1+*"

-}
displayNumeric :: (Show i) => Print (Numeric i)
displayNumeric = \case
 NumericCreature body -> displayBody body
 NumericLoyalty  x    -> show' x

displayBody :: (Show i) => Print (Body i)
displayBody Body{..} = mconcat
  [ displayNumericExpression _power
  , "/"
  , displayNumericExpression _toughness
  ]

displayNumericExpression :: (Show i) => Print (NumericExpression i)
displayNumericExpression = \case
  SimpleNumeric     x -> displayNumericLiteral x
  BinaryNumeric o x y -> mconcat
    [ displayNumericLiteral   x
    , displayNumericOperation o
    , displayNumericLiteral   y
    ]

displayNumericLiteral :: (Show i) => Print (NumericLiteral i)
displayNumericLiteral = \case
  NumericConstant i -> show' i
  NumericWildcard   -> "*"

displayNumericOperation :: Print NumericOperation
displayNumericOperation = \case
  NumericAddition    -> "+"
  NumericSubtraction -> "-"

----------------------------------------

displayChroma :: Print Chroma
displayChroma = chroma2text

displayColor :: Print Color
displayColor = color2text 

-- displayChroma :: Print Chroma
-- displayChroma = \case
--  Color color -> color
--  Colorless   -> 'C'

-- displayColorIdentity :: Print ColorIdentity
-- displayColorIdentity (ColorIdentity hue) = hue2text hue

-- displayColorIndication :: Print ColorIndication
-- displayColorIndication (ColorIndication color) = color2text color

chroma2text :: Chroma -> String
chroma2text = chroma2letter > (:[])  -- char2text

color2text :: Color -> String
color2text = color2letter > (:[])  -- char2text

chroma2letter :: Chroma -> Char
chroma2letter = \case
  Color color -> color2letter color
  Colorless   -> 'C'
  SnowMana    -> 'S' 
  Energy      -> 'E'

color2letter :: Color -> Char
color2letter = \case
   White -> 'W'
   Blue  -> 'U'
   Black -> 'B'
   Red   -> 'R'
   Green -> 'G'

----------------------------------------

displayManaSymbol :: (Show i) => Print (ManaSymbol i)
displayManaSymbol = \case

 ChromaSymbol     c -> displayChromaSymbol      c
 MonoHybridSymbol c -> displayMonoHybridSymbol c
 PhyrexianSymbol  c -> displayPhyrexianSymbol  c
 HybridSymbol    cs -> displayGuildSymbol      cs
 GenericSymbol    i -> displayGenericSymbol    i
 VariableSymbol     -> "{X}" -- braces "X" 

displayChromaSymbol :: Print Chroma
displayChromaSymbol = chroma2text > surround "{" "}"

displayColorSymbol :: Print Color
displayColorSymbol = color2text > surround "{" "}"

displayGenericSymbol :: (Show i) => Print i
displayGenericSymbol = show > toS > surround "{" "}"

displayMonoHybridSymbol :: Print Color
displayMonoHybridSymbol c = "{2/" <> t <> "}"
  where
  t = color2text c

displayPhyrexianSymbol :: Print Color
displayPhyrexianSymbol c = "{P" <> t <> "}"
  where
  t = color2text c
   
displayGuildSymbol :: Print Guild
displayGuildSymbol
  = fromGuild'
  > fmap color2text   -- e.g. ["U","G"]
  > intercalate "/"   -- e.g. "U/G"
  > surround "{" "}"  -- e.g. "{U/G}"

---------------------------------------

-- | in ascending (canonical, i.e. WUBRG) order
fromGuild' :: Guild -> [Color]
fromGuild' = fromGuild > pair2list > sort
 where
 pair2list (x,y) = [x,y]

-- | in ascending (canonical, i.e. WUBRG) order,
-- followed by TODO. 
fromSlice' :: Slice -> [Color]
fromSlice' = fromSlice > triplet2list > sort --TODO
 where
 triplet2list (x,y,z) = [x,y,z]

-- | in ascending (canonical, i.e. WUBRG) order
fromNephilim' :: Nephilim -> [Color]
fromNephilim' = fromNephilim > quadruplet2list > sort
 where
 quadruplet2list (w,x,y,z) = [w,x,y,z]

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
 Bant   -> (Green,White,Blue)
 Esper  -> (White,Blue,Black)
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
 Growth     -> (Green, White, Blue, Black)
                                                                    ----------------------------------------

displayWatermark :: Print Watermark
displayWatermark = \case  
  AbzanWatermark                 -> "Abzan"
  AgentsOfSNEAKWatermark         -> "Agents of S.N.E.A.K."
  AtarkaWatermark                -> "Atarka"
  AzoriusWatermark               -> "Azorius"
  BlackWatermark                 -> "Black"
  BlueWatermark                  -> "Blue"
  BorosWatermark                 -> "Boros"
  ColorlessWatermark             -> "Colorless"
  CrossbreedLabsWatermark        -> "Crossbreed Labs"
  DimirWatermark                 -> "Dimir"
  DromokaWatermark               -> "Dromoka"
  GoblinExplosioneersWatermark   -> "Goblin Explosioneers"
  GolgariWatermark               -> "Golgari"
  GreenWatermark                 -> "Green"
  GruulWatermark                 -> "Gruul"
  IzzetWatermark                 -> "Izzet"
  JeskaiWatermark                -> "Jeskai"
  KolaghanWatermark              -> "Kolaghan"
  LeagueOfDastardlyDoomWatermark -> "League of Dastardly Doom"
  MarduWatermark                 -> "Mardu"
  MirranWatermark                -> "Mirran"
  OjutaiWatermark                -> "Ojutai"
  OrderOfTheWidgetWatermark      -> "Order of the Widget"
  OrzhovWatermark                -> "Orzhov"
  PhyrexianWatermark             -> "Phyrexian"
  PlaneswalkerWatermark          -> "Planeswalker"
  RakdosWatermark                -> "Rakdos"
  RedWatermark                   -> "Red"
  SelesnyaWatermark              -> "Selesnya"
  SilumgarWatermark              -> "Silumgar"
  SimicWatermark                 -> "Simic"
  SultaiWatermark                -> "Sultai"
  TemurWatermark                 -> "Temur"
  WhiteWatermark                 -> "White"

----------------------------------------

displayRarity :: Print Rarity
displayRarity = \case
  Common   -> "C"
  Uncommon -> "U"
  Rare     -> "R"
  Mythic   -> "M"
                    
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

displayFace :: Print Layout
displayFace = \case
 Aftermath   -> "aftermath"
 DoubleFaced -> "doublefaced"
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
 
displayFrame :: Print Frame 
displayFrame = \case
 OldFrame         -> "old"
 TimeshiftedFrame -> "timeshifted"
 NewFrame         -> "new"
 FutureFrame      -> "future"

displayBorder :: Print Border
displayBorder = \case
 BlackBordered    -> "black"
 WhiteBordered    -> "white"
 SilverBordered   -> "silver"

----------------------------------------

{-|

@
= 'language2abbreviation'
@

>>> displayLanguage English
"en"

-}
displayLanguage :: Print Language
displayLanguage = language2abbreviation > toS

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
  e  = toS$ _qEdition  & edition2abbreviation
  l  = toS$ l' & maybe "" (\t -> "/" <> t)
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

displaySupertype :: Print Supertype
displaySupertype = show
{-
displaySupertype = \case
   Basic
   Legendary
   Snow
   Ongoing
   World
-}

displayBaseType :: Print BaseType
displayBaseType = show

{-
displayBaseType = \case
   Instant
   Sorcery
   Land
   Artifact
   Enchantment
   Creature
   Planeswalker
   Conspiracy
-}

----------------------------------------
