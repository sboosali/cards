#!/bin/bash
########################################

ghcid  --directory="./cards-frontend/"   --reload="./sources/"   --restart="./cards-frontend.cabal"   --project="cards-frontend"   --outputfile="./ghcid.txt"  --command 'cabal new-repl cards-frontend'

#NOTE cabal new-repl -fdevelop cards-frontend
#
# [nix-shell:~/haskell/cards]$ cabal new-repl -fdevelop cards-frontend
# cabal: Cannot open a repl for multiple components at once. The target
# 'cards-frontend' refers to the package cards-frontend-0.0.0 which includes the
# libraries cards-frontend-development and cards-frontend.

#NOTE unsupported: --ghci-options -ferror-spans"

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
