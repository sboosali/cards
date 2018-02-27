#!/bin/bash
set -e
########################################

ARGUMENTS=

########################################

jq '.[] |= {cards: .cards | map({ subtypes })}' AllSets-x.json | tr -s ' ' | sort|uniq| grep -P '^[a-zA-Z\s"]+$' | cut -d'"' -f2 > all-subtypes.txt

cat all-subtypes.txt

