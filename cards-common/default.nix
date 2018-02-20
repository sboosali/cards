{ mkDerivation, aeson, base, binary, bytestring, containers
, deepseq, exceptions, file-embed, hashable, lens, mtl, spiros
, stdenv, template-haskell, text, time, transformers
, unordered-containers
}:
mkDerivation {
  pname = "cards-common";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base binary bytestring containers deepseq exceptions
    file-embed hashable lens mtl spiros template-haskell text time
    transformers unordered-containers
  ];
  executableHaskellDepends = [ base ];
  homepage = "http://github.com/sboosali/cards#readme";
  description = "TODO";
  license = stdenv.lib.licenses.bsd3;
}
