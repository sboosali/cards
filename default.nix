
########################################
### "Imports"...
let 

nixpkgs = import <nixpkgs> {};
haskell = nixpkgs.haskell.lib;

reflex-platform = import ./reflex-platform {};
  # `reflex-platform` uses a pinned/older `nixpkgs` version.

inherit (builtins)
 fromJSON readFile baseNameOf;

inherit (nixpkgs)
 pkgs;

inherit (pkgs)   
 fetchFromGitHub;

in
########################################
### UTILITIES...
let

# utility functions
skipTests       = haskell.dontCheck; 
dropUpperBounds = haskell.doJailbreak;

#:: String -> Path -> 
execCabal2nix = options: src:
  nixpkgs.runCommand "cabal2nix" {
    buildCommand = ''
      cabal2nix ${options} file://"${src}" >"$out"
    '';
    buildInputs = with nixpkgs; [
      cabal2nix
    ];
  } "";

in
########################################
### Haskell Dependencies...
let

# Source repositories
sources = {

  # reflex = fetchFromGitHub {
  #   owner           = "reflex-frp";
  #   repo            = "reflex";
  #   rev             = "8e0177ff28c25436452dba1222cbf8d1a20424fd";
  #   fetchSubmodules = true;
  #   sha256          = "1f0xhwq4wvf5c6w8qhvpcn30jaxxq29s2x3iy8bml3a65fpvj0sh";
  # };

};

# "Megarepos",
# repositories which have multiple packages as subdirectories.
megarepos = {

   jsaddle = fetchFromGitHub {
      owner  = "ghcjs";
      repo   = "jsaddle";
      rev    = "e77d303880917a66e593dc2d0d5c8718cfb085c4"; 
               # "Make jsaddle-warp compatible with websockets-0.12"
               # continuous-integration build isn't broken
      sha256 =
        "143r9nfglkydhp6rl0qrsyfpjnxfj04fhn96cf8hkx2mk09baa01";
    }; # https://github.com/ghcjs/jsaddle

  reflex-dom = fetchFromGitHub {
    owner           = "reflex-frp";
    repo            = "reflex-dom"; 
    rev             = "f4820df681b177fb950eb180aa97a81f855bd2aa";
    sha256          =
       "0wv8xwr4bv2zb8qz3kf7nq2ixjg2hmyccxppgpwis3wmjai89frk";
  };

};

# subrepositories of a megarepo
subrepos = {

   jsaddle = { 
    path    = megarepos.jsaddle; 
    subpath = "jsaddle";
   };

   jsaddle-warp = { 
    path    = megarepos.jsaddle; 
    subpath = "jsaddle-warp";
   };

   jsaddle-webkit2gtk = { 
    path    = megarepos.jsaddle; 
    subpath = "jsaddle-webkit2gtk";
   };

};

#NOTES  
# jsaddle-warp        - runs JSaddle in a warp server with a web browser connected to it.
# jsaddle-webkit2gtk  - runs JSaddle in a WebKitGTK window.
# jsaddle-wkwebview   - runs JSaddle in a WKWebView on iOS or macOS.
# jsaddle-clib        - C interface used to run JSaddle on Android using JNI.

########################################

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

 # local "imports"
 inherit (pkgs)
  fetchgit 
 ;

 # helpers for getting haskell packages from different standard locations:
 # * the local filesystem
 # * hackage
 # * tarballs from anywhere
 # * sources on github. 
 # *
 #
 cabal2nix = name: source: 
             self.callCabal2nix name source;

 hackage   = name: version:
             self.callHackage name version;

 local     = path:
             self.callPackage path; 
 nix       = local; 
           # an alias (the distinct name is only for clarity)

 # `prefetched` meaning `nix-prefetch-*` in particular. 
 prefetched = path:
             let
              # basename-of-path
              n = baseNameOf path;
              # output from nix-prefetch-git
              o =
               fromJSON (readFile path);
              # input for fetchgit
              i = {
               inherit (o)
                 url rev sha256;
              };
              # package-derivation
              p = fetchgit i;
             in
             cabal2nix n p;                
#             self.callPackage p; 

 github    = o:
             cabal2nix o.repo (pkgs.fetchFromGitHub o); 

             # o ::
             #      { owner           :: String
             #        repo            :: String
             #        rev             :: String
             #        fetchSubmodules :: Bool
             #        sha256          :: String
             #      } 

 subrepository = {path, subpath}:
                 nix
                   (execCabal2nix "--subpath ${subpath}" path);

 # override the package without overriding any dependencies
 cabal2nix_     = name: source:   cabal2nix  name source  {};
 hackage_       = name: version:  hackage    name version {};
 nix_           = path:           nix        path         {};
 local_         = path:           local      path         {};
 prefetched_    = path:           prefetched path         {};
 github_        = o:              github     o            {};
 subrepository_ = paths:          subrepository paths     {};

 # more (local) utilities
 haskell = pkgs.haskell.lib; 
 #
 skipTests         = haskell.dontCheck; 
 skipDocumentation = haskell.dontHaddock;
 skipBenchmarks    = haskell.dontBenchmark;
 dropUpperBounds   = haskell.doJailbreak;
 #
 quicken   = p:
  skipDocumentation (skipTests ( p));
 loosen    = p:
  skipDocumentation (skipTests (skipBenchmarks p));
 dependsOn = package: dependencies: 
  haskell.addBuildDepends package dependencies;
 #
 inherit (haskell);
 #

 in

 {
   ########################################
   # Add Haskell Packages Below           #
   ######################################## 

    #TODO
    # make the development build quicker and more reliable
    # (haddock in particular is both slow, and fails to parse valid sources)
    cards-common        = quicken super.cards-common;
    cards-backend       = quicken super.cards-backend;
    cards-frontend      = quicken super.cards-frontend;
    cards-desktop-linux = quicken super.cards-desktop-linux;

    # spiros = github_ {
    #   owner  = "sboosali";
    #   repo   = "spiros";
    #   rev    = "f6c86509cfa1b198c5deb4b89b3dadf6184ea1d0"; 
    #   sha256 = "0bvxgp1mvlpwzr9qdpjs20qs4i813wnhrsl3nq25r8v68x6fblhk";
    # };
    #   # NOTE
    #   # latest needs ghc-8.2.2
    #   # rev "2b7517f27242863ba153bc045dd269b348df05aa" 

    spiros = local_ ../spiros;

    memo = local_ ../Haskell-Memoize;

    # spiros = local_ ../spiros;
     # ^ needed `protolude`, not anymore
    # protolude = prefetched_ ./protolude.json; 
     #
     # Preprocessing library for clock-0.7.2..
     # Clock.hsc:44:0: warning: "hsc_alignment" redefined
     # In file included from dist/build/System/Clock_hsc_test0.c:1:0:
     # /nix/store/kf23n3922ikr69lhz7ag29mda7rrykmw-ghc-8.2.1/lib/ghc-8.2.1/template-hsc.h:91:0: note: this is the location of the previous definition
     #  #define hsc_alignment(x...)                                           \
     #  ^
     # System/Clock.hsc:44 directive let cannot be handled in cross-compilation mode
     # builder for ‘/nix/store/8614br7l58ifr54x81pbnbxax5bxf0nq-clock-0.7.2-aarch64-unknown-linux-android.drv’ failed with exit code 1
     # building path(s) ‘/nix/store/6ixvivjic4j1qnvfd8zld6a9v2jf52qg-clock-0.7.2-arm-unknown-linux-androideabi’
     # cannot build derivation ‘/nix/store/bgwlga9jlcx9bgygd8mq4znhdnjfq5a5-spiros-0.0.1-aarch64-unknown-linux-android.drv’: 1 dependencies couldn't be built
     # cannot build derivation ‘/nix/store/wjgj9a89fwyij2m3klfxmphrfh68fnr5-cards-frontend-0.0.0-aarch64-unknown-linux-android.drv’: 1 dependencies couldn't be built
     # killing process 13427
     # cannot build derivation ‘/nix/store/vyzdvvkdybm6xd0fr2hn5zajw5mzg0lr-android-app.drv’: 1 dependencies couldn't be built

    jsaddle            = loosen (subrepository_ subrepos.jsaddle);
    jsaddle-warp       = loosen (subrepository_ subrepos.jsaddle-warp);
    jsaddle-webkit2gtk = loosen (subrepository_ subrepos.jsaddle-webkit2gtk);

    reflex-dom-core = loosen super.reflex-dom-core;
    reflex-dom      = loosen super.reflex-dom;
     
    # protolude = hackage "protolude" "0.2.1" {};
    # 

    unicode-show = skipTests super.unicode-show;

     # Linking dist/build/unicode-show-test/unicode-show-test ...
     # running tests
     # Running 1 test suites...
     # Test suite unicode-show-test: RUNNING...
     # individual representations test:
     #   ushow "\1589\1576\1575\1581 \1575\1604\1582\1610\1585" == "صباح الخير": [OK]
     #   ushow "\128518\128149>\955\\=\128024" == "😆💕>λ\\=🐘": [OK]
     #   ushow "\28450\&6" == "漢6": [OK]
     #   ushow " 7" == " 7": [OK]
     #   ushow "\25913\n\34892" == "改\n行": [OK]
     #   ushow "\19979\19968\31449\na\ri\ta\22269\38469\26426\22330" == "下一站\na\ri\ta国际机场": [OK]
     #   ushow "\SOH\SO\&H" == "\SOH\SO\&H": [OK]
     # read . ushow == id:
     #   read . ushow == id, for String: [OK, passed 100 tests]
     #   read . read . ushow . ushow == id, for String: [OK, passed 100 tests]
     #   read . ushow == id, for some crazy Unicode type: [OK, passed 100 tests]
     #   read . ushow == id, for some crazy Unicode type: [Failed]
     # *** Failed! Exception: 'Prelude.read: no parse' (after 1 test): 
     # ""
     # ""
     # (used seed -5100929916581787240)
     #   read . ushow == id, for compound type: [OK, passed 100 tests]
     # 
     #          Properties  Test Cases  Total       
     #  Passed  4           7           11          
     #  Failed  1           0           1           
     #  Total   5           7           12          
     # Test suite unicode-show-test: FAIL
     # Test suite logged to: dist/test/unicode-show-0.1.0.2-unicode-show-test.log

    reflex-vinyl = haskell.dontHaddock (local_ ../reflex-vinyl);
    # sources/DOM/Attribute/Singletons.hs:285:3: error:
    #     parse error on input ‘-- ^ Identifies the currently active element when DOM focus is on a composite widget, textbox, group, or application. ’
    # 5) in 'Reflex.Vinyl'


    # reflex-vinyl = github_ {
    #   owner  = "sboosali";
    #   repo   = "reflex-vinyl";
    #   rev    = ""; 
    #   sha256 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
    # };

    vinyl = github_ {
      owner  = "VinylRecords";
      repo   = "Vinyl";
      rev    = "a5ffd10fbc747c5366ae9806e61bf45f78c3eb33";  # "0.7.0"
      sha256 = "18wa9qpxw4za7h9dav4xfjq876z3dkpmxnpzfvv1bvq0hpldsrip";
    };
    # sources/DOM/Extra.hs:16:1: error:
    #     Failed to load interface for ‘Data.Vinyl.CoRec’
    #     Perhaps you meant Data.Vinyl.Core (from vinyl-0.5.3)
    #
    # vinyl = hackage_ "vinyl" "0.7.0";
    # sed: can't read /nix/store/hcqlfq5rcshf31k9cnrvgzan9jdf37cy-all-cabal-hashes-2b0bf3ddf8b75656582c1e45c51caa59458cd3ad-src/1/vinyl/0.7.0/vinyl.json: No such file or directory
    # cabal2nix: nix-prefetch-url: createProcess: runInteractiveProcess: exec: does not exist (No such file or directory)


    # reflex-dom-contrib = self.callPackage ./deps/reflex-dom-contrib.nix {};
    reflex-dom-contrib = github_ {
      owner  = "reflex-frp";
      repo   = "reflex-dom-contrib";
      rev    = "b47f90c810c838009bf69e1f8dacdcd10fe8ffe3"; 
      fetchSubmodules = true;
      sha256 =
        "0yvjnr9xfm0bg7b6q7ssdci43ca2ap3wvjhshv61dnpvh60ldsk9";
    };


   # $ nix-prefetch-git  https://github.com/ghcjs/jsaddle  > jsaddle.json
   # jsaddle = prefetched_ ./jsaddle.json;

   # jsaddle = github_ {
   #    owner  = "ghcjs";
   #    repo   = "jsaddle"
   #    rev    = ""; 
   #    # fetchsubmodules = true;
   #    sha256 =
   #      "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx";
   #  };        

    megaparsec = github {
      owner  = "mrkkrp";
      repo   = "megaparsec";
      rev    = "140fe937954c9e9c4801778b3d427524d9fb8a23"; 
      fetchSubmodules = true;
      sha256 = "116w7hy2hp61j29kyafsfq5fx6s12954vnpxb6jgipvlcwnjwq3l";
    } {
      # inherit (self) parser-combinators; 
    };
      # reflex-platform has megaparsec-5.3.1
      # megaparsec = hackage_ "megaparsec" "6.4.0";
      # ERROR '''can't read /nix/store/hcqlfq5rcshf31k9cnrvgzan9jdf37cy-all-cabal-hashes-2b0bf3ddf8b75656582c1e45c51caa59458cd3ad-src/1/megaparsec/6.4.0/megaparsec.json: No such file or directory'''

    parser-combinators = github_ {
      owner  = "mrkkrp";
      repo   = "parser-combinators";
      rev    = "9a03215c6d0a1bf5d81328f446d40226ef3d77ea"; 
      fetchSubmodules = true;
      sha256 = "0yl9ribw1lff1h8jgq8mi11ivb8psj86p5dbdkirfw0mla0kydr8";
    };
      # '''Configuring megaparsec-6.3.0...
      # Setup: Encountered missing dependencies:
      # parser-combinators >=0.4 && <1.0'''

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

  # reflex-dom-core = let
  #   megarepo = repositories.reflex-dom;
  #   in
  #   (skipTests (dropUpperBounds
  #     (nix_ (execCabal2nixSubpath "reflex-dom-core" megarepo))));

  # reflex-dom = let
  #   megarepo = repositories.reflex-dom;
  #   in
  #   (skipTests (dropUpperBounds
  #     (nix_ (execCabal2nix "--subpath reflex-dom -fuse-warp" megarepo))));
  
  # jsaddle-dom        = loosen (github_ {
  #     owner  = "ghcjs";
  #     repo   = "jsaddle-dom";
  #     rev    = "d6a85de23b5644f1e39a590ec706c90be5aeaa51"; 
  #     sha256 =
  #       "198zmd50jvyn8sahn7ysdqy105h8vs6xw0wpa3wlc9rp867y2zi4";
  # });

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

see


*/
myExecutablesWith = ghc: let 
 
 systemExecutables = with nixpkgs; [
  cabal-install
  chromium
  inotify-tools
  graphviz # dot, tred
 ];

 haskellExecutables = with ghc; [
  ghcid
#  nix-tags-haskell  
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

    cards-desktop-linux = ./cards-desktop-linux;

  };

  shells = {
    ghc   = [ "cards-common" "cards-backend" "cards-frontend" "cards-desktop-linux" ];
    ghcjs = [ "cards-common"                 "cards-frontend" ];
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
