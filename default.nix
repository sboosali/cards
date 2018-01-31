
########################################
### "Imports"...
let 

nixpkgs = import <nixpkgs> {};
haskell = nixpkgs.haskell.lib;

reflex-platform = import ./reflex-platform {};
  # `reflex-platform` uses a pinned/older `nixpkgs` version.

in
########################################
### Haskell Dependencies...
let

# utility functions
skipTests       = haskell.dontCheck; 
dropUpperBounds = haskell.doJailbreak;

/* customize the environment's haskell packages. 

we can override it with newer versions of haskell packages. 
`reflex-platform`s packages are older, 
and some newly released ones are missing, 
since `reflex-platform` pins an older `nixpkgs`, 
and `ghcjs` uses `ghc-8.0`, the

NOTE: `self` v `super`
self.callCabal2nix amd self.callHackage both call self.callPackage,
which is correct, since we want to mix in any siblings that are being overriden too.

*/

myOverlaysWith = pkgs: self: super: let

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

 # override the package without overriding any dependencies
 cabal2nix_ = name: source:   cabal2nix name source  {};
 hackage_   = name: version:  hackage   name version {};
 local_     = path:           local     path         {};
 github_    = o:              github    o            {};

 in

 {
   ########################################
   # Add Haskell Packages Below           #
   ######################################## 

    spiros = github_ {
      owner  = "sboosali";
      repo   = "spiros";
      rev    = "f6c86509cfa1b198c5deb4b89b3dadf6184ea1d0"; 
      sha256 = "0bvxgp1mvlpwzr9qdpjs20qs4i813wnhrsl3nq25r8v68x6fblhk";
    };
      # NOTE
      # latest needs ghc-8.2.2
      # rev "2b7517f27242863ba153bc045dd269b348df05aa" 

 /* 

  # You can use `callHackage` and `callCabal2nix` 
  # to bump package versions or build them from GitHub. 
  # e.g.

    spiros = self.spiros_loose;

    spiros_loose   = skipTests (dropUpperBounds self.spiros_github);
    spiros_local   = local ../spiros {
    };
    spiros_hackage = hackage "spiros" "0.0.0" {
    };
    spiros_github  = github {
      owner  = "sboosali";
      repo   = "spiros";
      rev    = "f6c86509cfa1b198c5deb4b89b3dadf6184ea1d0"; 
      # "2b7517f27242863ba153bc045dd269b348df05aa" 
      # latest needs ghc-8.2.2
      sha256 = 
         "0bvxgp1mvlpwzr9qdpjs20qs4i813wnhrsl3nq25r8v68x6fblhk";
    } {
    };
 */

 };

in
########################################
### Executables..
let

/*

NOTE
`systemExecutables` uses `nixpkgs`, not some given `pkgs`,
because `reflex-platform` pins an older `nixpkgs`, but
the executables of system packages don't depend on 
haskell packages. and even those that are haskell executables,
like pandoc or cabal-install, are build-time dependencies,
not run-time dependencies, and thus can be looser. 

*/
myExecutablesWith = ghc: let 
 
 systemExecutables = with nixpkgs; [
  cabal-install
  chromium
  inotify-tools
 ];

 haskellExecutables = with ghc; [
  
 ];
 
 in

 systemExecutables ++ haskellExecutables;

in
########################################
### 
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

  tools = myExecutablesWith;

  withHoogle = false;

  overrides = myOverlaysWith pkgs;
    # overrides ? _: _: {}
    # :: PackageSet -> PackageSet -> { <package name> :: Derivation }

})
########################################
# also see ./notes.txt

/* NOTES

*/
