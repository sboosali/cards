#!/bin/bash
set -e

PREFIX=./dist-ghcjs/build/x86_64-linux/ghcjs-0.2.1/cards-frontend-0.0.0/c/example-cards-frontend/build/example-cards-frontend

NAME=example-cards-frontend.jsexe

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

# cp "$PREFIX/example-cards-frontend.jsexe" 
rsync -a "$PREFIX/$NAME/" "js/$NAME"
find "js/$NAME"

echo
echo "========================================"
echo "Opening In Browser..."
echo "========================================"
echo

chromium "js/$NAME/index.html" 2>/dev/null
