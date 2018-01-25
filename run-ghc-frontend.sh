#!/bin/bash
PREFIX="dist-newstyle/build/x86_64-linux/ghc-8.0.2/cards-frontend-0.0.0/c/example-cards-frontend/build/example-cards-frontend"

nix-shell -A shells.ghc --run "cabal new-build cards-frontend"
chromium http://localhost:3911/
nix-shell -A shells.ghc --run "$PREFIX/example-cards-frontend"

# no `cabal new-run` yet
# ./dist-newstyle/build/PACKAGE-VERSION/build/EXECUTABLE/EXECUTABLE
