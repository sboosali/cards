#!/bin/bash
set -e

nix-build  -o ghc-frontend-result  -A ghc.cards-frontend

tree ghc-frontend-result
