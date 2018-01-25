#!/bin/bash

# https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md

#

mkdir cards
cd cards
git init
git submodule add https://github.com/reflex-frp/reflex-platform

h-new cards-common   Cards.Common   Cards/Common   && cd ..
h-new cards-backend  Cards.Backend  Cards/Backend  && cd ..
h-new cards-frontend Cards.Frontend Cards/Frontend && cd ..

rm cards-backend/cabal.project 
rm cards-common/cabal.project 
rm cards-frontend/cabal.project 

touch default.nix # edit...

#

touch cabal.project # edit...

touch cabal-ghcjs.project # edit...

#
