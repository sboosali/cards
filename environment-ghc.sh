#!/bin/bash
set -e

COMMAND='./new-build-common.sh'

nix-shell -A shells.ghc --show-trace --run "$COMMAND"

nix-shell -A shells.ghc --show-trace

