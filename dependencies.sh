#!/bin/bash
set -e
########################################

########################################

mkdir -p metadata

DOTFILE=metadata/dependencies.dot

ghc-pkg dot > "$DOTFILE"

cat "$DOTFILE"

########################################

# ghc-pkg dot | tred | dot -Tpdf >pkgs.pdf

