#!/bin/sh

# parallel
nix-shell  -A shells.ghc    --run 'cabal new-build common'  &
nix-shell  -A shells.ghcjs  --run 'cabal new-build common'  &
wait
