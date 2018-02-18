#!/bin/bash
set -e
#########################################

#########################################

./build-ghc-common.sh  # +RTS -K2G -RTS 

./new-run-common.sh "$@"


