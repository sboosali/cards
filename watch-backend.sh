#!/bin/bash
set -e
########################################


########################################

nix-shell -A shells.ghc --run 'ghcid --directory="./cards-backend/" --reload="./sources/" --restart="./cards-backend.cabal" --project="cards-backend" --command "cabal new-repl cards-backend" --outputfile=./ghcid.txt'

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


# TARGET="cards-backend"
# CABAL_COMMAND="cabal new-repl $TARGET"
# GHCID_COMMAND="ghcid --command $CABAL_COMMAND"
# NIX_COMMAND="nix-shell -A shells.ghc $GHCID_COMMAND"
#
# echo ${NIX_COMMAND}
# eval ${NIX_COMMAND}
