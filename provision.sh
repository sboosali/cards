#!/bin/bash

nix-shell -A shells.ghc --run ""

# SHELL_FILE="$1"  # blank is okay, it defaults
# nix-shell "$SHELL_FILE" --arg doBenchmark true --run "./configure.sh"
