#!/bin/bash

###########################################

# GHCID_COMMAND='cabal repl fltkhs-reflex' 
# # GHCID_COMMAND='ghci -fobject-code -ferror-spans -ignore-dot-ghci' 
# ghcid -o ghcid.txt --command "${GHCID_COMMAND}"
# 
# can't load .so/.DLL for libHSfltkhs: /nix/store/p032q22qigxr838sn# undefined symbol: Fl_Adjuster_New

# $ ghcid --command 'cat' --test 'cabal build' 

# function set-timestamps () {
#   TIMESTAMP=`date '+%T'`
# }

########################################

BUILD_COMMAND='nix-shell -A shells.ghcjs --run 'cabal new-build cards-frontend --ghc-options="-fno-code -fobject-code -ferror-spans" 1> /dev/null; cabal new-build cards-frontend > /dev/null'
# true build after type checking

# TODO also watch .cabal

WATCHED_DIRECTORY=./cards-frontend/sources/

WATCHED_EXTENSION="hs"
WATCHED_REGEX='\.'"$WATCHED_EXTENSION"'$'
IGNORED_REGEX='(.*~|\.#.*)' # emacs temp files

INOTIFY_EVENTS='modify,create,delete'

# stubs
OLD_TIMESTAMP="xx:xx:xx"
NEW_TIMESTAMP="yy:yy:yy"

RUNNING_BUILD=""

function debounce () {
  #TODO does `cabal build` touch (pseudo-modifies) each file? 
  # events are duplicated retriggering
  # oh, cabal build is too slow, the build should be queued

  # seconds resolution
  NEW_TIMESTAMP=`date '+%T'`

  # string equality
  if [ "$NEW_TIMESTAMP" = "$OLD_TIMESTAMP" ]; then
     EXIT_CODE=1 
     # too soon, exit code failure
  else
     EXIT_CODE=0
     # a second boundary was crossed, later enough, exit code success
  fi

  OLD_TIMESTAMP="$NEW_TIMESTAMP"  
  return $EXIT_CODE
}

########################################

echo '[Building once initially]'
echo

# don't exit if the build fails
# eval "$BUILD_COMMAND" || true 
eval "$BUILD_COMMAND" & 
RUNNING_BUILD="$!" 
echo

echo '['Watching "$WATCHED_DIRECTORY"']'
echo

inotifywait --monitor --event $INOTIFY_EVENTS --recursive $WATCHED_DIRECTORY  | grep "$WATCHED_REGEX" --line-buffered  | grep -E -v "$IGNORED_REGEX" --line-buffered  | while read -r directory event filename; do

  if debounce; then
  filepath="${directory}${filename}"
  echo
  echo -en "\ec" # hard clear-screen
  echo '----------------------------------------'
  echo ${filepath}
  echo ${event}
  echo

  if [ ! -z "${RUNNING_BUILD}" ]; then
     echo "kill ${RUNNING_BUILD}" 
     kill "${RUNNING_BUILD}" || true
       # don't fail if the build already finished. 
       # kill the last build (if it's currently running),
       # since the source has changed,
       # to avoid multiple redundant simultaneous builds. 
  fi
  eval "$BUILD_COMMAND" & 
   # `eval` needed for a command with spaces
   # `&` needed, i.e. async, to avoid sabotaging `debounce`.
   #   echo -en "\ff" doesn't work
  RUNNING_BUILD="$!" 
    # the process identifier of the most recently backgrounded process

  fi
done

########################################
