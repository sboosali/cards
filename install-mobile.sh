#!/bin/bash
set -e

case $(uname) in
 ("Linux")  
     echo "[installing android app, since we're on Linux...]"
     ./install-android.sh 
     ;;
 ("Darwin")
     echo "[installing iOS app, since we're on OSX...]"
     ./install-ios.sh 
     ;;
 (*) 
     echo "FAILURE [architecture must be Linux or OSX]" 
     exit
     ;;
esac
