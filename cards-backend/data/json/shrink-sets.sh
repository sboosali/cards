#!/bin/bash
set -e
########################################

########################################

jq '.[] |= {cards: .cards | map({ artist, cmc, manaCost, name, rarity, text, type })}' "$1" "$2"

########################################

# e.g.
# ./shrink-cards.sh AllSets-x.json AllSets-y.json 
# jq '.[] |= {cards: .cards | map({ artist, cmc, manaCost, name, rarity, text, type })}' AllSets-x.json > AllSets-y.json 

