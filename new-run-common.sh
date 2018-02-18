#!/bin/bash
set -e
#########################################

TARGET=cards-common
EXECUTABLE=example-cards-common

#########################################

(cd cards-common && "../dist-newstyle/build/x86_64-linux/ghc-8.0.2/$TARGET-0.0.0/c/$EXECUTABLE/build/$EXECUTABLE/$EXECUTABLE" "$@")

#########################################

#NOTE for the data-files to be read, 
# this script must be relative to the package (sub)directory, i.e. not the project directory. 

#########################################
