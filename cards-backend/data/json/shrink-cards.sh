#!/bin/bash
set -e
########################################

ARGUMENTS=

########################################

jq --compact-output --sort-keys '.[] | {name,text}' AllCards-x.json > CardNamesTexts.json 

jq --compact-output --join-output --sort-keys '.[] | {name,text}' AllCards-x.json > CardNamesTexts.OneLine.json 

#  jq --raw-output '[.name,.text] | join("\t")' CardNamesTexts.json 

# TODO wrap to make valid .hs
jq -c '[.name,.text]' CardNamesTexts.json | sed 's/\[/\ , (/g' | sed 's/\]/\)/g' | sed '0,/,/{s/,/\[/}' > AllCards.hs

