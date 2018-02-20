# reflex-dom-contrib.nix
{ mkDerivation, aeson, base, base64-bytestring, bifunctors
, bytestring, containers, data-default, exception-transformers
, fetchgit, ghcjs-dom, http-types, lens, mtl, random
, readable, ref-tf, reflex, reflex-dom-core, safe, stdenv, stm
, string-conv, text, time, transformers, uri-bytestring
}:
mkDerivation {
  pname = "reflex-dom-contrib";
  version = "0.5.1.0";
  src = fetchgit {
    url = "https://github.com/reflex-frp/reflex-dom-contrib/";
    sha256 = "0yvjnr9xfm0bg7b6q7ssdci43ca2ap3wvjhshv61dnpvh60ldsk9";
    rev = "b47f90c810c838009bf69e1f8dacdcd10fe8ffe3";
  };
  libraryHaskellDepends = [
    aeson base base64-bytestring bifunctors bytestring containers
    data-default exception-transformers ghcjs-dom http-types 
    lens mtl random readable ref-tf reflex reflex-dom-core safe stm
    string-conv text time transformers uri-bytestring
  ];
  homepage = "https://github.com/reflex-frp/reflex-dom-contrib";
  description = "A place to experiment with infrastructure and common code for reflex applications";
  license = stdenv.lib.licenses.bsd3;
}
