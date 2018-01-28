#!/bin/bash
set -e
#########################################

PACKAGE=cards-frontend-0.0.0

EXECUTABLE=example-cards-frontend

#########################################

NAME="$EXECUTABLE.jsexe"

PREFIX="./dist-ghcjs/build/x86_64-linux/ghcjs-0.2.1/$PACKAGE/c/$EXECUTABLE/build/$EXECUTABLE"

echo
echo "========================================"
echo "Building GHCJS Frontend..."
echo "========================================"
echo

./build-ghcjs.sh

echo
echo "========================================"
echo "Copying / Post-Processing JavaScript..."
echo "========================================"
echo

# cp "$PREFIX/$EXECUTABLE.jsexe" 
rsync -a "$PREFIX/$NAME/" "js/$NAME"
find "js/$NAME"

echo
echo "========================================"
echo "Opening In Browser..."
echo "========================================"
echo

chromium "js/$NAME/index.html" 2>/dev/null
