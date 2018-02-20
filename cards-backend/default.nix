{ mkDerivation, base, bytestring, cards-common, containers, deepseq
, exceptions, hashable, lens, mtl, reflex-dom, spiros, stdenv, text
, transformers, unordered-containers
}:
mkDerivation {
  pname = "cards-backend";
  version = "0.0.0";
  src = ./.;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base bytestring cards-common containers deepseq exceptions hashable
    lens mtl reflex-dom spiros text transformers unordered-containers
  ];
  homepage = "http://github.com/sboosali/cards#readme";
  description = "TODO";
  license = stdenv.lib.licenses.bsd3;
}
