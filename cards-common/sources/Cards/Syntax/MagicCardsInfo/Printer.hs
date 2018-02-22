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
