{ mkDerivation, base, blaze-html, bytestring, cards-common, clay
, containers, deepseq, directory, exceptions, filepath, ghcjs-dom
, hashable, jsaddle, jsaddle-warp, lens, megaparsec, memo, mtl
, reflex, reflex-dom, reflex-dom-contrib, reflex-dom-core
, reflex-vinyl, spiros, stdenv, string-conv, text, thyme, time
, transformers, unordered-containers, wai-middleware-static, warp
, websockets
}:
mkDerivation {
  pname = "cards-frontend";
  version = "0.0.0";
  src = ./.;
  configureFlags = [ "-fdevelop" ];
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    base blaze-html bytestring cards-common clay containers deepseq
    directory exceptions filepath ghcjs-dom hashable jsaddle
    jsaddle-warp lens megaparsec memo mtl reflex reflex-dom
    reflex-dom-contrib reflex-dom-core reflex-vinyl spiros string-conv
    text thyme time transformers unordered-containers
    wai-middleware-static warp websockets
  ];
  executableHaskellDepends = [
    base lens reflex-dom reflex-dom-core text
  ];
  homepage = "http://github.com/sboosali/cards#readme";
  description = "TODO";
  license = stdenv.lib.licenses.bsd3;
}
