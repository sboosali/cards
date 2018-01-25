# also see ./notes.txt

########################################
let 

reflex-platform = import ./reflex-platform {};

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

})

########################################
/*
*/
