#!/bin/bash
set -eu
########################################
########################################

# e.g.
# ./generate-transitive-dependencies.sh webapp
# ./generate-transitive-dependencies.sh desktop
# ./generate-transitive-dependencies.sh android
# ./generate-transitive-dependencies.sh backend

# $ ls * | grep result
# android-result
# desktop-result
# result
# emacs-result
# result
# webapp-result

########################################
TARGET="$1"
 # ^ "In bash you can use set -u which causes bash to exit on failed parameter expansion."

########################################
nix-store -q --graph "$TARGET-result/" > "dependencies-of-$TARGET.txt"

cat "dependencies-of-$TARGET.txt"
########################################
