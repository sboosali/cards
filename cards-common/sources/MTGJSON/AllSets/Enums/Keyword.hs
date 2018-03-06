{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|

@
Differences between keyword abilities, keyword actions, and ability words:

For example, the keyword ability enchant, as in Enchant creature, states a restriction on the valid targets for an aura. The related keyword action attach describes the process of physically putting such an aura onto its target.
@


@
From the Comprehensive Rules (Rivals of Ixalan (January 19, 2018))

702. Keyword Abilities
702.1. Most abilities describe exactly what they do in the card’s rules text. Some, though, are very common or would require too much space to define on the card. In these cases, the object lists only the name of the ability as a “keyword”; sometimes reminder text summarizes the game rule.

702.2. Deathtouch
702.3. Defender
702.4. Double Strike
702.5. Enchant
702.6. Equip
702.7. First Strike
702.8. Flash
702.9. Flying
702.10. Haste
702.11. Hexproof
702.12. Indestructible
702.13. Intimidate
702.14. Landwalk
702.15. Lifelink
702.16. Protection
702.17. Reach
702.18. Shroud
702.19. Trample
702.20. Vigilance
702.21. Banding
702.22. Rampage
702.23. Cumulative Upkeep
702.24. Flanking
702.25. Phasing
702.26. Buyback
702.27. Shadow
702.28. Cycling
702.29. Echo
702.30. Horsemanship
702.31. Fading
702.32. Kicker
702.33. Flashback
702.34. Madness
702.35. Fear
702.36. Morph
702.37. Amplify
702.38. Provoke
702.39. Storm
702.40. Affinity
702.41. Entwine
702.42. Modular
702.43. Sunburst
702.44. Bushido
702.45. Soulshift
702.46. Splice
702.47. Offering
702.48. Ninjutsu
702.49. Epic
702.50. Convoke
702.51. Dredge
702.52. Transmute
702.53. Bloodthirst
702.54. Haunt
702.55. Replicate
702.56. Forecast
702.57. Graft
702.58. Recover
702.59. Ripple
702.60. Split Second
702.61. Suspend
702.62. Vanishing
702.63. Absorb
702.64. Aura Swap
702.65. Delve
702.66. Fortify
702.67. Frenzy
702.68. Gravestorm
702.69. Poisonous
702.70. Transfigure
702.71. Champion
702.72. Changeling
702.73. Evoke
702.74. Hideaway
702.75. Prowl
702.76. Reinforce
702.77. Conspire
702.78. Persist
702.79. Wither
702.80. Retrace
702.81. Devour
702.82. Exalted
702.83. Unearth
702.84. Cascade
702.85. Annihilator
702.86. Level Up
702.87. Rebound
702.88. Totem Armor
702.89. Infect
702.90. Battle Cry
702.91. Living Weapon
702.92. Undying
702.93. Miracle
702.94. Soulbond
702.95. Overload
702.96. Scavenge
702.97. Unleash
702.98. Cipher
702.99. Evolve
702.100. Extort
702.101. Fuse
702.102. Bestow
702.103. Tribute
702.104. Dethrone
702.105. Hidden Agenda
702.106. Outlast
702.107. Prowess
702.108. Dash
702.109. Exploit
702.110. Menace
702.111. Renown
702.112. Awaken
702.113. Devoid
702.114. Ingest
702.115. Myriad
702.116. Surge
702.117. Skulk
702.118. Emerge
702.119. Escalate
702.120. Melee
702.121. Crew
702.122. Fabricate
702.123. Partner
702.124. Undaunted
702.125. Improvise
702.126. Aftermath
702.127. Embalm
702.128. Eternalize
702.129. Afflict
702.130. Ascend
@

-}
module MTGJSON.AllSets.Enums.Keyword where

import MTGJSON.Extra

import Control.Lens (makePrisms)

----------------------------------------

newtype Keyword = Keyword Text
 deriving (Show,Read,Eq,Ord,Generic,NFData,Hashable,IsString)

makePrisms ''Keyword

----------------------------------------

_Deathtouch :: Keyword
_Deathtouch = "Deathtouch"

_Defender :: Keyword
_Defender = "Defender"

_DoubleStrike :: Keyword
_DoubleStrike = "Double Strike"

_Enchant :: Keyword
_Enchant = "Enchant"

_Equip :: Keyword
_Equip = "Equip"

_FirstStrike :: Keyword
_FirstStrike = "First Strike"

_Flash :: Keyword
_Flash = "Flash"

_Flying :: Keyword
_Flying = "Flying"

_Haste :: Keyword
_Haste = "Haste"

_Hexproof :: Keyword
_Hexproof = "Hexproof"

_Indestructible :: Keyword
_Indestructible = "Indestructible"

_Intimidate :: Keyword
_Intimidate = "Intimidate"

_Landwalk :: Keyword
_Landwalk = "Landwalk"

_Lifelink :: Keyword
_Lifelink = "Lifelink"

_Protection :: Keyword
_Protection = "Protection"

_Reach :: Keyword
_Reach = "Reach"

_Shroud :: Keyword
_Shroud = "Shroud"

_Trample :: Keyword
_Trample = "Trample"

_Vigilance :: Keyword
_Vigilance = "Vigilance"

_Banding :: Keyword
_Banding = "Banding"

_Rampage :: Keyword
_Rampage = "Rampage"

_CumulativeUpkeep :: Keyword
_CumulativeUpkeep = "Cumulative Upkeep"

_Flanking :: Keyword
_Flanking = "Flanking"

_Phasing :: Keyword
_Phasing = "Phasing"

_Buyback :: Keyword
_Buyback = "Buyback"

_Shadow :: Keyword
_Shadow = "Shadow"

_Cycling :: Keyword
_Cycling = "Cycling"

_Echo :: Keyword
_Echo = "Echo"

_Horsemanship :: Keyword
_Horsemanship = "Horsemanship"

_Fading :: Keyword
_Fading = "Fading"

_Kicker :: Keyword
_Kicker = "Kicker"

_Flashback :: Keyword
_Flashback = "Flashback"

_Madness :: Keyword
_Madness = "Madness"

_Fear :: Keyword
_Fear = "Fear"

_Morph :: Keyword
_Morph = "Morph"

_Amplify :: Keyword
_Amplify = "Amplify"

_Provoke :: Keyword
_Provoke = "Provoke"

_Storm :: Keyword
_Storm = "Storm"

_Affinity :: Keyword
_Affinity = "Affinity"

_Entwine :: Keyword
_Entwine = "Entwine"

_Modular :: Keyword
_Modular = "Modular"

_Sunburst :: Keyword
_Sunburst = "Sunburst"

_Bushido :: Keyword
_Bushido = "Bushido"

_Soulshift :: Keyword
_Soulshift = "Soulshift"

_Splice :: Keyword
_Splice = "Splice"

_Offering :: Keyword
_Offering = "Offering"

_Ninjutsu :: Keyword
_Ninjutsu = "Ninjutsu"

_Epic :: Keyword
_Epic = "Epic"

_Convoke :: Keyword
_Convoke = "Convoke"

_Dredge :: Keyword
_Dredge = "Dredge"

_Transmute :: Keyword
_Transmute = "Transmute"

_Bloodthirst :: Keyword
_Bloodthirst = "Bloodthirst"

_Haunt :: Keyword
_Haunt = "Haunt"

_Replicate :: Keyword
_Replicate = "Replicate"

_Forecast :: Keyword
_Forecast = "Forecast"

_Graft :: Keyword
_Graft = "Graft"

_Recover :: Keyword
_Recover = "Recover"

_Ripple :: Keyword
_Ripple = "Ripple"

_SplitSecond :: Keyword
_SplitSecond = "Split Second"

_Suspend :: Keyword
_Suspend = "Suspend"

_Vanishing :: Keyword
_Vanishing = "Vanishing"

_Absorb :: Keyword
_Absorb = "Absorb"

_AuraSwap :: Keyword
_AuraSwap = "Aura Swap"

_Delve :: Keyword
_Delve = "Delve"

_Fortify :: Keyword
_Fortify = "Fortify"

_Frenzy :: Keyword
_Frenzy = "Frenzy"

_Gravestorm :: Keyword
_Gravestorm = "Gravestorm"

_Poisonous :: Keyword
_Poisonous = "Poisonous"

_Transfigure :: Keyword
_Transfigure = "Transfigure"

_Champion :: Keyword
_Champion = "Champion"

_Changeling :: Keyword
_Changeling = "Changeling"

_Evoke :: Keyword
_Evoke = "Evoke"

_Hideaway :: Keyword
_Hideaway = "Hideaway"

_Prowl :: Keyword
_Prowl = "Prowl"

_Reinforce :: Keyword
_Reinforce = "Reinforce"

_Conspire :: Keyword
_Conspire = "Conspire"

_Persist :: Keyword
_Persist = "Persist"

_Wither :: Keyword
_Wither = "Wither"

_Retrace :: Keyword
_Retrace = "Retrace"

_Devour :: Keyword
_Devour = "Devour"

_Exalted :: Keyword
_Exalted = "Exalted"

_Unearth :: Keyword
_Unearth = "Unearth"

_Cascade :: Keyword
_Cascade = "Cascade"

_Annihilator :: Keyword
_Annihilator = "Annihilator"

_LevelUp :: Keyword
_LevelUp = "Level Up"

_Rebound :: Keyword
_Rebound = "Rebound"

_TotemArmor :: Keyword
_TotemArmor = "Totem Armor"

_Infect :: Keyword
_Infect = "Infect"

_BattleCry :: Keyword
_BattleCry = "Battle Cry"

_LivingWeapon :: Keyword
_LivingWeapon = "Living Weapon"

_Undying :: Keyword
_Undying = "Undying"

_Miracle :: Keyword
_Miracle = "Miracle"

_Soulbond :: Keyword
_Soulbond = "Soulbond"

_Overload :: Keyword
_Overload = "Overload"

_Scavenge :: Keyword
_Scavenge = "Scavenge"

_Unleash :: Keyword
_Unleash = "Unleash"

_Cipher :: Keyword
_Cipher = "Cipher"

_Evolve :: Keyword
_Evolve = "Evolve"

_Extort :: Keyword
_Extort = "Extort"

_Fuse :: Keyword
_Fuse = "Fuse"

_Bestow :: Keyword
_Bestow = "Bestow"

_Tribute :: Keyword
_Tribute = "Tribute"

_Dethrone :: Keyword
_Dethrone = "Dethrone"

_HiddenAgenda :: Keyword
_HiddenAgenda = "Hidden Agenda"

_Outlast :: Keyword
_Outlast = "Outlast"

_Prowess :: Keyword
_Prowess = "Prowess"

_Dash :: Keyword
_Dash = "Dash"

_Exploit :: Keyword
_Exploit = "Exploit"

_Menace :: Keyword
_Menace = "Menace"

_Renown :: Keyword
_Renown = "Renown"

_Awaken :: Keyword
_Awaken = "Awaken"

_Devoid :: Keyword
_Devoid = "Devoid"

_Ingest :: Keyword
_Ingest = "Ingest"

_Myriad :: Keyword
_Myriad = "Myriad"

_Surge :: Keyword
_Surge = "Surge"

_Skulk :: Keyword
_Skulk = "Skulk"

_Emerge :: Keyword
_Emerge = "Emerge"

_Escalate :: Keyword
_Escalate = "Escalate"

_Melee :: Keyword
_Melee = "Melee"

_Crew :: Keyword
_Crew = "Crew"

_Fabricate :: Keyword
_Fabricate = "Fabricate"

_Partner :: Keyword
_Partner = "Partner"

_Undaunted :: Keyword
_Undaunted = "Undaunted"

_Improvise :: Keyword
_Improvise = "Improvise"

_Aftermath :: Keyword
_Aftermath = "Aftermath"

_Embalm :: Keyword
_Embalm = "Embalm"

_Eternalize :: Keyword
_Eternalize = "Eternalize"

_Afflict :: Keyword
_Afflict = "Afflict"

_Ascend :: Keyword
_Ascend = "Ascend"

----------------------------------------

allKeywordAbilities :: [Keyword]
allKeywordAbilities =
  [ _Deathtouch
  , _Defender
  , _DoubleStrike
  , _Enchant
  , _Equip
  , _FirstStrike
  , _Flash
  , _Flying
  , _Haste
  , _Hexproof
  , _Indestructible
  , _Intimidate
  , _Landwalk
  , _Lifelink
  , _Protection
  , _Reach
  , _Shroud
  , _Trample
  , _Vigilance
  , _Banding
  , _Rampage
  , _CumulativeUpkeep
  , _Flanking
  , _Phasing
  , _Buyback
  , _Shadow
  , _Cycling
  , _Echo
  , _Horsemanship
  , _Fading
  , _Kicker
  , _Flashback
  , _Madness
  , _Fear
  , _Morph
  , _Amplify
  , _Provoke
  , _Storm
  , _Affinity
  , _Entwine
  , _Modular
  , _Sunburst
  , _Bushido
  , _Soulshift
  , _Splice
  , _Offering
  , _Ninjutsu
  , _Epic
  , _Convoke
  , _Dredge
  , _Transmute
  , _Bloodthirst
  , _Haunt
  , _Replicate
  , _Forecast
  , _Graft
  , _Recover
  , _Ripple
  , _SplitSecond
  , _Suspend
  , _Vanishing
  , _Absorb
  , _AuraSwap
  , _Delve
  , _Fortify
  , _Frenzy
  , _Gravestorm
  , _Poisonous
  , _Transfigure
  , _Champion
  , _Changeling
  , _Evoke
  , _Hideaway
  , _Prowl
  , _Reinforce
  , _Conspire
  , _Persist
  , _Wither
  , _Retrace
  , _Devour
  , _Exalted
  , _Unearth
  , _Cascade
  , _Annihilator
  , _LevelUp
  , _Rebound
  , _TotemArmor
  , _Infect
  , _BattleCry
  , _LivingWeapon
  , _Undying
  , _Miracle
  , _Soulbond
  , _Overload
  , _Scavenge
  , _Unleash
  , _Cipher
  , _Evolve
  , _Extort
  , _Fuse
  , _Bestow
  , _Tribute
  , _Dethrone
  , _HiddenAgenda
  , _Outlast
  , _Prowess
  , _Dash
  , _Exploit
  , _Menace
  , _Renown
  , _Awaken
  , _Devoid
  , _Ingest
  , _Myriad
  , _Surge
  , _Skulk
  , _Emerge
  , _Escalate
  , _Melee
  , _Crew
  , _Fabricate
  , _Partner
  , _Undaunted
  , _Improvise
  , _Aftermath
  , _Embalm
  , _Eternalize
  , _Afflict
  , _Ascend
  ]

----------------------------------------
