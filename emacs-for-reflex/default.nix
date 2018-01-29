{ nixpkgs ? import <nixpkgs> {}
, emacs ? nixpkgs.emacs25
}: 

/* NOTES...

Arguments:

    The emacs version/configuration is `emacs25` by default.

Querying:

    nix-env -f "<nixpkgs>" -qaP -A emacsPackagesNg.melpaPackages | grep ...
    nix-env -f "<nixpkgs>" -qaP -A emacsPackagesNg.orgPackages | grep ...

Building: 

    nix-build default.nix

Running:

    ./result/bin/emacs

Build and Launch:

    nix-build default.nix && ./result/bin/emacs

    # aka

    ../editor.sh

See:

    https://nixos.org/wiki/Emacs_configuration
*/

########################################
let

emacsPackages = nixpkgs.emacsPackagesNgGen emacs;

# withRepositories :: [PackageSet] -> [PackageSet] -> [PackageSet] -> [PackageSet] -> [PackageSet] 
emacsWith = withRepositories: 
 emacsPackages.emacsWithPackages (epkgs: 
   withRepositories epkgs.melpaPackages epkgs.melpaStablePackages epkgs.elpaPackages epkgs.orgPackages);

/* e.g.
 * emacsWith (melpa: stable: elpa: org: [ melpa.dante ... ]); 
 */

# takes callback/continuation for scoping
# withMelpa :: [PackageSet] -> [PackageSet] 
emacsWithMelpa = withMelpa: 
 (nixpkgs.emacsPackagesNgGen emacs).emacsWithPackages (epkgs: 
   withMelpa epkgs.melpaPackages);

           # (you can also use...)
           # epkgs.melpaStablePackages
           # epkgs.orgPackages
           # epkgs.elpaPackages

in
########################################

emacsWithMelpa (melpa: with melpa; [

 dante
 haskell-mode 
 flycheck
 projectile

 nix-mode

 multi-term
 magit          # <C-x g>
 helm
 real-auto-save

 use-package
 dash
 s

 ])

########################################
