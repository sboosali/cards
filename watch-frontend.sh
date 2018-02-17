#!/bin/bash
########################################

emacsclient ./cards-frontend/ghcid.txt &

nix-shell -A shells.ghc --run './ghcid-frontend.sh'

########################################
