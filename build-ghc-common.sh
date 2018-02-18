#!/bin/sh

nix-shell  -A shells.ghc    --run 'cabal new-build cards-common --ghc-options "+RTS -K8G -RTS"'

# -K2G
# set the maximum heap size to 8G


