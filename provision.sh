#!/bin/bash

nix-shell -A shells.ghc --run "" --show-trace

# SHELL_FILE="$1"  # blank is okay, it defaults
# nix-shell "$SHELL_FILE" --arg doBenchmark true --run "./configure.sh"
