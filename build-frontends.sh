#!/bin/sh

# parallel
nix-shell  -A shells.ghc    --run ./new-build-frontend-ghc.sh    &
nix-shell  -A shells.ghcjs  --run ./new-build-frontend-ghcjs.sh  &
wait
