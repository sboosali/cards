#!/bin/bash
set -e
########################################

ARGUMENTS=

########################################

jq '.[] |= {cards: .cards | map({ watermark })}' AllSets-x.json | grep watermark | sort | uniq | cut -d '"' -f4 | grep -v -e '^[[:space:]]*$'

