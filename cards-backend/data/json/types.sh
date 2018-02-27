#!/bin/bash
set -e
########################################

ARGUMENTS=

########################################

jq '.[] |= {cards: .cards | map({ types })}' AllSets-x.json | tr -s ' ' | sort|uniq| grep -P '^[a-zA-Z\s"]+$' | cut -d'"' -f2

jq '.[] |= {cards: .cards | map({ supertypes })}' AllSets-x.json | tr -s ' ' | sort|uniq| grep -P '^[a-zA-Z\s"]+$' | tr -d '"' | tr -d ' '

