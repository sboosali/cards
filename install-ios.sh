#!/bin/bash
set -e

nix-build  -o ios-result  -A ios.cards-frontend

tree ios-result

# TODO I don't have a mac to test this
