#!/bin/bash
set -e

nix-build  -o desktop-result  -A ghc.cards-desktop-linux

tree desktop-result

./desktop-result/bin/card-search-desktop-linux

