#!/bin/sh
nix-shell  -A shells.ghc  --run 'cabal new-build cards-backend'

