#!/bin/bash
set -e
########################################

ghcid  --reload=static  --reload="./cards-frontend/sources/"  --command "cabal new-repl card-search-development"  -T "reload"

########################################

# ghcid  --directory="./"  --reload=static  --reload="./cards-frontend/executables/"  --reload="./cards-frontend/sources/"   --restart="./cards-frontend/cards-frontend.cabal"   --project="./"   --outputfile="./ghcid.txt"  --command "cabal new-repl card-search-development"  -T "reload"
