#!/bin/bash
set -e
########################################
cabal2nix .           > default.nix
cabal2nix . -fdevelop > develop.nix
