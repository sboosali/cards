#!/bin/bash
set -e
#######################################
# e.g.

# $ ./editor.sh --debug-init
# a.k.a.
# $ ./emacs-result/bin/emacs -q --load emacs-for-reflex/init.el --debug-init

# $ ./editor.sh --file ./cards-frontend/sources/Cards/Frontend/Example.hs

# $ ./editor.sh ./cards-frontend/sources/Cards/Frontend/Example.hs

########################################
EMACS_OPTIONS="$@"

########################################

EMACS_INIT_DIR=emacs-for-reflex
EMACS_RESULTS_DIR=emacs-result

nix-build "$EMACS_INIT_DIR/default.nix" -o "$EMACS_RESULTS_DIR" --show-trace

"./$EMACS_RESULTS_DIR/bin/emacs" -q --load "$EMACS_INIT_DIR/init.el" ${EMACS_OPTIONS} 2>/dev/null

########################################

# --file file, --find-file file, --visit file
#         The same as specifying file directly as an argument.

# +number 
#         Go  to  the line specified by number (do not insert a space between the
#         "+" sign and the number).  This applies only to the  next  file  specified.

# +line:column
#         Go to the specified line and column.

# `-q`
# (do not load an initialization file)

# --script file
#         Run file as an Emacs Lisp script.

# -f function, --funcall function
#         Execute the lisp function function.

# -l file, --load file
#         Load the lisp code in the file file.

# --eval expr, --execute expr
#         Evaluate the Lisp expression expr.



########################################