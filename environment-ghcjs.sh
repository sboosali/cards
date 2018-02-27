#!/bin/bash
set -e

COMMAND='./new-build-common.sh'

nix-shell -A shells.ghcjs --show-trace --run "$COMMAND"

nix-shell -A shells.ghcjs --show-trace

