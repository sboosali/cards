#!/bin/bash
set -e
########################################

GHCID_FILE=./cards-common/ghcid.txt

echo '...' > "$GHCID_FILE"
emacsclient "$GHCID_FILE" &

########################################

nix-shell -A shells.ghc --run 'ghcid --directory="./cards-common/" --reload="./sources/" --restart="./cards-common.cabal" --project="cards-common" --command "cabal new-repl cards-common" --outputfile=./ghcid.txt'

########################################

#  -c --command=COMMAND  Command to run (defaults to ghci or cabal repl)
#  -T --test=EXPR        Command to run after successful loading
#     --reload=PATH      Reload when the given file or directory contents
#                        change (defaults to none)
#     --restart=PATH     Restart the command when the given file or directory
#                        contents change (defaults to .ghci and any .cabal file)
#  -C --directory=DIR    Set the current directory
#  -o --outputfile=FILE  File to write the full output to
#     --ignore-loaded    Keep going if no files are loaded. Requires --reload

