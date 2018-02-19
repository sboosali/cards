#!/bin/bash
set -e
########################################

#NOTE

########################################

nix-shell -A shells.ghc --run './new-build-develop.sh'

#nix-shell -A shells.ghc --run './new-run-develop.sh'

nix-shell -A shells.ghc --run './ghcid-develop.sh'

########################################
# NOTE
  # no `cabal new-run` yet
  # "pattern" is ./dist-newstyle/build/PACKAGE-VERSION/build/EXECUTABLE/EXECUTABLE
  # (bash array for intentional globbing to a single filepath)


# PREFIX=(dist-newstyle/build/*/*/cards-frontend-0.0.0/c/card-search-development/build/card-search-development)
# # shellcheck disable=SC2086
# "$PREFIX/card-search-development"

########################################
