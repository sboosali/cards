{ mkDerivation, base, cards-frontend, jsaddle-webkit2gtk, stdenv }:
mkDerivation {
  pname = "cards-desktop-linux";
  version = "0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base cards-frontend jsaddle-webkit2gtk
  ];
  homepage = "http://github.com/sboosali/cards#readme";
  description = "TODO";
  license = stdenv.lib.licenses.gpl3;
}
