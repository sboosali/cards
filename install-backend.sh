#!/bin/bash
set -e

nix-build  -o backend-result  -A ghc.cards-backend

tree backend-result

# ./backend-result/bin/example-cards-backend 
