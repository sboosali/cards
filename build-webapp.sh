#!/bin/bash
set -e

nix-shell  -A shells.ghcjs  --run  "cabal  --project-file=cabal-ghcjs.project  --builddir=dist-ghcjs new-build  cards-frontend"
