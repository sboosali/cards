cat ignore/RIXSetsY.txt | sed 's/,/\n/g' | grep -E '\\[0-9]+' 

cat ignore/RIXSetsY.txt | sed 's/\\226\\128\\148/\\8212/g' | sed 's/,/\n/g' | grep -E -v '\\8212' | grep -E '\\[0-9]+' 

\8212


\n      {\n        \"name\": \"Huatli
 Radiant Champion\"
\n        \"id\": \"02867303bf360d7451c66f59fac1bbd1fb3fdf74\"
\n        \"manaCost\": \"{2}{G}{W}\"
\n        \"cmc\": 4
\n        \"colors\": [\n          \"White\"
\n          \"Green\"\n        ]
\n        \"colorIdentity\": [\n          \"W\"
\n          \"G\"\n        ]
\n        \"supertypes\": [\n          \"Legendary\"\n        ]
\n        \"types\": [\n          \"Planeswalker\"\n        ]
\n        \"subtypes\": [\n          \"Huatli\"\n        ]
\n        \"rarity\": \"Mythic Rare\"
\n        \"text\": \"+1: Put a loyalty counter on Huatli
 Radiant Champion for each creature you control.\\n\226\136\146\&1: Target creature gets +X/+X until end of turn
 where X is the number of creatures you control.\\n\226\136\146\&8: You get an emblem with \\\"Whenever a creature enters the battlefield under your control
 you may draw a card.\\\"\"
\n        \"flavor\": null

