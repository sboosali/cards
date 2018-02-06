#!/bin/bash
set -e
#########################################

TARGET=cards-common
EXECUTABLE=example-cards-common

#########################################

"dist-newstyle/build/x86_64-linux/ghc-8.0.2/$TARGET-0.0.0/c/$EXECUTABLE/build/$EXECUTABLE/$EXECUTABLE"

# ^ NOTE no `cabal new-run`

#########################################
