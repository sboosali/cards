#!/bin/bash
########################################

GHCID_FILE="./cards-frontend/ghcid.txt"

echo '...' > "$GHCID_FILE"
emacsclient ./cards-frontend/ghcid.txt &

########################################

nix-shell -A shells.ghc --run './ghcid-frontend.sh'

########################################
