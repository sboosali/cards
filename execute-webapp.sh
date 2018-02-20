#!/bin/bash
set -e
#########################################

TARGET=cards-frontend
EXECUTABLE=card-search-webapp

#########################################

chromium "./dist-ghcjs/build/x86_64-linux/ghcjs-0.2.1/$TARGET-0.0.0/c/$EXECUTABLE/build/$EXECUTABLE/$EXECUTABLE.jsexe/index.html" "$@" 2>/dev/null

#########################################
