#!/bin/bash
set -e

nix-build  -o android-result  -A android.cards-frontend

tree android-result
