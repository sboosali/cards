
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-|

MagicCards.Info Syntax: <https://magiccards.info/syntax.html>


Simple Queries
====================

Name
--------------------
Birds of Paradise
"Birds of Paradise"
!Anger (Match the full name)

Rules Text (Oracle)
--------------------
o:Flying
o:"First strike"
o:{T} o:"add one mana of any color"
(new) o:"whenever ~ deals combat damage"

Types (Oracle)
--------------------
t:angel
t:"legendary angel"
t:basic
t:"arcane instant"

Colors
--------------------
c:w (Any card that is white)
c:wu (Any card that is white or blue)
c:wum (Any card that is white or blue, and multicolored)
c!w (Cards that are only white)
c!wu (Cards that are only white or blue, or both)
c!wum (Cards that are only white and blue, and multicolored)
c!wubrgm (Cards that are all five colors)
c:m (Any multicolored card)
c:l or c:c (Lands and colorless cards)

Color Identity
--------------------
ci:wu (Any card that is white or blue, but does not contain any black, red or green mana symbols)
Color Indicator:
(new) in:wu (Any card that is white or blue according to the color indicator.)

Mana Cost
--------------------
mana=3G (Spells that cost exactly 3G, or split cards that can be cast with 3G)
mana>=2WW (Spells that cost at least two white and two colorless mana)
mana<GGGGGG (Spells that can be cast with strictly less than six green mana)
mana>=2RR mana<=6RR (Spells that cost two red mana and between two and six colorless mana)
(new) mana>={2/R}
(new) mana>={W/U}
(new) mana>={UP}

Power, Toughness, Converted Mana Cost
--------------------
pow>=8
tou<pow (All combinations are possible)
cmc=7
(new) cmc>=*

Rarity
--------------------
r:mythic

Format
--------------------
f:standard (or block, extended, vintage, classic, legacy, modern, commander)
banned:extended (or legal, restricted)

Artist
--------------------
a:"rk post"

Edition
--------------------
e:al/en (Uses the abbreviations that are listed on the sitemap)
(new) e:al,be (Cards that appear in Alpha or Beta)
(new) e:al+be (Cards that appear in Alpha and Beta)
(new) e:al,be -e:al+be (Cards that appear in Alpha or Beta but not in both editions)
(new) year<=1995 (Cards printed in 1995 and earlier)

Is
--------------------
is:split, is:flip
is:vanilla (Creatures with no card text)
is:old, is:new, is:future (Old/new/future card face)
is:timeshifted
is:funny, not:funny (Unglued/Unhinged/Happy Holidays Promos)
is:promo (Promotional cards)
is:promo is:old (Promotional cards with the original card face)
(new) is:permanent, is:spell
(new) is:black-bordered, is:white-bordered, is:silver-bordered
(new) has:foil

Language
--------------------
l:de, l:it, l:jp (Uses the abbreviations that are listed on the sitemap)


Complex Queries
====================

any Simple Query can be combined with other Simple Queries via Logical Operators to make... Complex Queries! 

e.g. "a or b", "a not b", "a -b", "a (b or c)", "a or (b c)", etc.


Logical Operators
====================

_ and _
_ _ (i.e. whitespace)

_ or _

not _
- _

( _ )


========================================
Example Queries


e.g. Saboteurs

o:"whenever ~" ((o:"deals damage to a" or o:"deals combat damage to a") (o:opponent or o:player)) or o:"attacks and isn't blocked")



-}
module Cards.Syntax.MagicCardsInfo.Types where

import Prelude.Spiros

----------------------------------------

{-| @magiccards.info@'s sytax

-}
data Syntax = Syntax
 { mciFreeText :: Maybe Text
 , mciFields   :: Map Text [Text]
 }

----------------------------------------

{-| @magiccards.info@'s behavior, features, predicates, etc.

-}

----------------------------------------
