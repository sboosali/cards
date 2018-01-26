#!/bin/bash
set -e
########################################
# NOTE
# ./watch-webapp.sh uses `jsaddle-warp` (via the cabal flag `-fjsaddle-warp`)
# ./develop-webapp.sh doesn't
# both use `ghc`
########################################

PREFIX=(dist-newstyle/build/*/*/cards-frontend-0.0.0/c/example-cards-frontend/build/example-cards-frontend)
  # TODO no `cabal new-run` yet
  # "pattern" is ./dist-newstyle/build/PACKAGE-VERSION/build/EXECUTABLE/EXECUTABLE
  # (bash array for intentional globbing to a single filepath)

# build
nix-shell -A shells.ghc --run "cabal new-build cards-frontend -fjsaddle-warp"

# open browser
(sleep 3 && chromium http://localhost:3911/) &

# launch development server
# shellcheck disable=SC2086
"$PREFIX/example-cards-frontend"

########################################
