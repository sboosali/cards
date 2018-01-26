#!/bin/sh

# parallel
nix-shell  -A shells.ghc    --run 'cabal new-build cards-frontend'  &
nix-shell  -A shells.ghcjs  --run 'cabal new-build cards-frontend'  &
wait
