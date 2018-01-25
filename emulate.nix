# { androidenv
# , example-cards-frontend
# }:

########################################
let 

reflex-platform = import ./reflex-platform {};
cards-project   = import ./default.nix;

nixpkgs = reflex-platform.nixpkgs;
androidenv = nixpkgs.pkgs.androidenv;

cards-frontend = cards-project.android.cards-frontend;

cards-frontend-config = { #TODO
    executableName = "example-cards-frontend";
    applicationId  = "org.example.cards_frontend";
    displayName    = "Example Android App";
};

in
########################################

androidenv.emulateApp {

  name            = "emulate-${cards-frontend-config.executableName}";
  package         = cards-frontend-config.applicationId;
  app             = cards-frontend;

  # name            = "emulate-${cards-frontend.executableName}";
  # package         = cards-frontend.applicationId;
  # app             = cards-android-app;

  platformVersion = "25";
    # $ nix-env -iA androidenv
    # installing ‘android-sdk-25.2.5’

  useGoogleAPIs   = true;
  activity        = ".HaskellActivity";
}
