#!/bin/bash
SHELL_FILE="$1"  # blank is okay, it defaults
nix-build "$SHELL_FILE" 
