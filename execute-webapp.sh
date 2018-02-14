#!/bin/bash
set -e

chromium ./dist-ghcjs/build/x86_64-linux/ghcjs-0.2.1/cards-frontend-0.0.0/c/example-cards-frontend/build/example-cards-frontend/example-cards-frontend.jsexe/index.html  2>/dev/null

#TODO
# chromium ./webapp-result/bin/example-cards-frontend.jsexe/index.html 2>/dev/null
