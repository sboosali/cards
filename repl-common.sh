#!/bin/bash
nix-shell -A shells.ghc --run 'cabal new-repl cards-common'

