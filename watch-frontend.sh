#!/bin/bash
########################################

########################################

nix-shell -A shells.ghc --run 'ghcid --directory="./cards-frontend/" --reload="./sources/" --restart="./cards-frontend.cabal" --project="cards-frontend" --command "cabal new-repl cards-frontend"'

#  -c --command=COMMAND  Command to run (defaults to ghci or cabal repl)
#  -T --test=EXPR        Command to run after successful loading
#     --reload=PATH      Reload when the given file or directory contents
#                        change (defaults to none)
#     --restart=PATH     Restart the command when the given file or directory
#                        contents change (defaults to .ghci and any .cabal file)
#  -C --directory=DIR    Set the current directory


# TARGET="cards-frontend"
# CABAL_COMMAND="cabal new-repl $TARGET"
# GHCID_COMMAND="ghcid --command $CABAL_COMMAND"
# NIX_COMMAND="nix-shell -A shells.ghc $GHCID_COMMAND"
#
# echo ${NIX_COMMAND}
# eval ${NIX_COMMAND}

########################################

# ghcid -o ghcid.txt

# CABAL_COMMAND=( cabal new-repl cards-frontend )
# ghcid --command "${CABAL_COMMAND[@]}"

# BAD
# nix-shell -A shells.ghc --run 'ghcid --reload="./cards-frontend/sources/" --restart="./cards-frontend/cards-frontend.cabal" --project="cards-frontend" --command "cabal new-repl cards-frontend"'
# nix-shell -A shells.ghc --run 'ghcid --directory="./cards-frontend/" --restart="./cards-frontend/cards-frontend.cabal" --project="cards-frontend" --command "cabal new-repl cards-frontend"'
# 
