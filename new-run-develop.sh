#!/bin/bash
set -e
#########################################

TARGET=cards-frontend
EXECUTABLE=card-search-development
ARGUMENTS="${@:1}"

#########################################

"./dist-newstyle/build/x86_64-linux/ghc-8.0.2/$TARGET-0.0.0/c/$EXECUTABLE/build/$EXECUTABLE/$EXECUTABLE" "$ARGUMENTS"

#########################################

#NOTE for the data-files to be read, 
# this script must be relative to the package (sub)directory, i.e. not the project directory. 

# e.g.
# ./new-run-frontend.sh card-search-development 

#########################################
