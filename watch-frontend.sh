#!/bin/bash
########################################

GHCID_FILE=./cards-frontend/ghcid.txt

echo '...' > "$GHCID_FILE"
emacsclient "$GHCID_FILE" &

########################################

DEVELOP=true nix-shell -A shells.ghc --run './ghcid-frontend.sh'
#TODO pass args explicitly via nix, not via env-vars

########################################
