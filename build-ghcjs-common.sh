#!/bin/sh

nix-shell  -A shells.ghcjs  --run 'cabal new-build cards-common'
