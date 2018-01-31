
########################################
let 

# imports
nixpkgs = import <nixpkgs> {};
haskell = nixpkgs.haskell.lib;

# utility functions
skipTests       = haskell.dontCheck; 
dropUpperBounds = haskell.doJailbreak;

# 
# cabal2nix = nixpkgs.haskellPackages.callCabal2nix;
# hackage   = nixpkgs.haskellPackages.callHackage
# nixpkgs.haskellPackages.callPackage

# haskell = nixpkgs.haskell.packages.ghc822;

# `reflex-platform` uses a pinned/older `nixpkgs` version.
reflex-platform = import ./reflex-platform {};

in
########################################
let

cardsOverlays = pkgs: self: super: let

 cabal2nix = name: source: 
             self.callCabal2nix name source;

 hackage   = name: version:
             self.callHackage name version;

 local     = path:
             self.callPackage path; 

 github    = o:
             cabal2nix o.repo (pkgs.fetchFromGitHub o); 

             # o ::
             #      { owner           :: String
             #        repo            :: String
             #        rev             :: String
             #        fetchSubmodules :: Bool
             #        sha256          :: String
             #      } 

 in

 {
 
 };

in
########################################
reflex-platform.project ({ pkgs, ... }: {

  packages = {
    cards-common   = ./cards-common;
    cards-backend  = ./cards-backend;
    cards-frontend = ./cards-frontend;
  };

  shells = {
    ghc   = ["cards-common" "cards-backend" "cards-frontend"];
    ghcjs = ["cards-common"                 "cards-frontend"];
  };
    # shells :: { <platform name> :: [PackageName] }

  android.cards-frontend = {
    executableName = "example-cards-frontend";
    applicationId  = "org.example.cards_frontend";
    displayName    = "Example Android App";
  };

  ios.cards-frontend = {
    executableName   = "example-cards-frontend";
    bundleIdentifier = "org.example.cards_frontend";
    bundleName       = "Example iOS App";
  };

  tools = ghc: with ghc; [
    pkgs.cabal-install
    pkgs.chromium
  ];

  withHoogle = false;

#  overrides = cardsOverlays pkgs self super;

  overrides = self: super: let
   # cabal2nix = self.callCabal2nix;
   # hackage   = self.callHackage;
   # file      = self.callPackage; 
   cabal2nix = super.callCabal2nix;
   hackage   = super.callHackage;
   local     = super.callPackage; 
   github    = o: cabal2nix o.repo (pkgs.fetchFromGitHub o); 

   in

   {
    spiros = skipTests (dropUpperBounds self.spiros_github);

    spiros_local   = super.callPackage   ../spiros         {};
    spiros_hackage = super.callHackage   "spiros"  "0.0.0" {};
    spiros_github  = cabal2nix "spiros" (pkgs.fetchFromGitHub {
      owner  = "sboosali";
      repo   = "spiros";
      rev    = "f6c86509cfa1b198c5deb4b89b3dadf6184ea1d0"; 
               # "2b7517f27242863ba153bc045dd269b348df05aa" needs ghc-8.2.2
      sha256 = "0bvxgp1mvlpwzr9qdpjs20qs4i813wnhrsl3nq25r8v68x6fblhk";
    }) {};
    #TODO use `super.callXXX`?

   # e.g. skipTests (dropUpperBounds )
   # https://github.com/cjdev/monad-persist

  };

})

########################################
/*
also see ./notes.txt
*/

  # overrides ? _: _: {}
  # :: PackageSet -> PackageSet ->  { <package name> :: Derivation }
  #
  # A function for overriding Haskell packages. You can use
  # `callHackage` and `callCabal2nix` to bump package versions or
  # build them from GitHub. e.g.
  #
  #     overrides = self: super: {
  #       lens = self.callHackage "lens" "4.15.4" {};
  #       free = self.callCabal2nix "free" (pkgs.fetchFromGitHub {
  #         owner = "ekmett";
  #         repo = "free";
  #         rev = "a0c5bef18b9609377f20ac6a153a20b7b94578c9";
  #         sha256 = "0vh3hj5rj98d448l647jc6b6q1km4nd4k01s9rajgkc2igigfp6s";
  #       }) {};
  #     };

