{ mkDerivation, base, blaze-html, bytestring, cards-common, clay
, containers, deepseq, exceptions, ghcjs-dom, hashable, lens
, megaparsec, memo, mtl, reflex, reflex-dom, reflex-dom-contrib
, reflex-dom-core, reflex-vinyl, spiros, stdenv, string-conv, text
, thyme, time, transformers, unordered-containers
}:
mkDerivation {
  pname = "cards-frontend";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base blaze-html bytestring cards-common clay containers deepseq
    exceptions ghcjs-dom hashable lens megaparsec memo mtl reflex
    reflex-dom reflex-dom-contrib reflex-dom-core reflex-vinyl spiros
    string-conv text thyme time transformers unordered-containers
  ];
  executableHaskellDepends = [
    base lens reflex-dom reflex-dom-core text
  ];
  homepage = "http://github.com/sboosali/cards#readme";
  description = "TODO";
  license = stdenv.lib.licenses.bsd3;
}
