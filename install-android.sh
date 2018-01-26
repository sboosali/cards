#!/bin/bash
set -e

nix-build  -o android-result  -A android.cards-frontend

tree android-result

# Manually Install App On Android Phone
du -h android-result/android-app-debug.apk | cut -f1
readlink -f android-result
nautilus android-result
