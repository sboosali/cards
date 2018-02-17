#!/bin/bash
nix-shell -A shells.ghc --run "cabal new-build cards-desktop-linux"
