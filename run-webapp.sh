#!/bin/bash
set -e

./build-webapp.sh

chromium ./webapp-result/bin/example-cards-frontend.jsexe/index.html 
