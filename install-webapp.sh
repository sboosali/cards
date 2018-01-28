#!/bin/bash
set -e

nix-build  -o webapp-result  -A ghcjs.cards-frontend

tree webapp-result

chromium ./webapp-result/bin/example-cards-frontend.jsexe/index.html 2>/dev/null
