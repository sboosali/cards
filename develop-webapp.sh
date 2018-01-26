#!/bin/bash
set -e
########################################
# NOTE
# ./watch-webapp.sh uses `jsaddle-warp`
# ./develop-webapp.sh doesn't
# both use `ghc`
########################################

PREFIX=(dist-newstyle/build/*/*/cards-frontend-0.0.0/c/example-cards-frontend/build/example-cards-frontend)
  # TODO no `cabal new-run` yet
  # "pattern" is ./dist-newstyle/build/PACKAGE-VERSION/build/EXECUTABLE/EXECUTABLE
  # (bash array for intentional globbing to a single filepath)

# build
nix-shell -A shells.ghc --run "cabal new-build cards-frontend -f-jsaddle-warp"

# shellcheck disable=SC2086
"$PREFIX/example-cards-frontend"

########################################
