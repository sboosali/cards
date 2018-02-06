#!/bin/bash
set -e

#NOTE https://stedolan.github.io/jq/manual/

########################################

ARGUMENTS=

########################################
PREFIX="cards-backend/data/json"

########################################

jq -c '{ LEA, ARN, ATQ, LEG, DRK, FEM, ICE, CHR, HML, ALL, MIR, VIS, POR, WTH, TMP, STH, PO2, EXO, USG, ULG, UDS, PTK, MMQ, NMS, PCY, INV, PLS, APC, ODY, TOR, JUD, ONS, LGN, SCG, MRD, DST, ("5DN"): .["5DN"], CHK, BOK, SOK, RAV, GPT, DIS, CSP, TSP, PLC, FUT, LRW, MOR, SHM, EVE, ALA, CON, ARB, M10, HOP, ZEN, WWK, ROE, M11, SOM, MBS, NPH, CMD, M12, ISD, DKA, PC2, AVR, M13, RTR, GTC, DDK, DGM, MMA, M14, THS, C13, BNG, JOU, CNS, M15, KTK, C14, FRF, DTK, ORI, BFZ, C15, OGW, W16, SOI, EMA, EMN, CN2, KLD, C16, AER, AKH, HOU, C17, XLN, RIX }' "$PREFIX/AllSets-x.json" > "$PREFIX/RealSets-x.json"

# NOTE `("5DN"): .["5DN"]` has a numeric prefix, and thus can't be an identifier.
# 105 sets

jq '.[] |= {code, name, cards: .cards | map({ name,id,manaCost,cmc,colors,colorIdentity,type,supertypes,types,subtypes,rarity,text,flavor,power,toughness,loyalty,artist,mciNumber,rulings,legalities })}' "$PREFIX/RealSets-x.json" > "$PREFIX/RealSets-y.json"

jq '{RIX,XLN}' "$PREFIX/RealSets-y.json" > "$PREFIX/RIXSets-y.json"

jq '.[] | [.]' "$PREFIX/RIXSets-y.json" > "$PREFIX/RIXSets-z.json"
#NOTE "If you have a filter X that produces four results, then the expression [X] will produce a single result, an array of four elements."

########################################

echo
echo "[Wrote]"
echo "* $PREFIX/RealSets-x.json"
echo "* $PREFIX/RealSets-y.json"
echo "*  $PREFIX/RIXSets-y.json"
echo "*  $PREFIX/RIXSets-z.json"
echo
echo "$ find $PREFIX/"
find "$PREFIX/"

########################################
