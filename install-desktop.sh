#!/bin/bash
set -e

nix-build  -o desktop-result  -A ghc.cards-frontend

tree desktop-result

./desktop-result/bin/example-cards-frontend 
