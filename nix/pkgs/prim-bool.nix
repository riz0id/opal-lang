{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib
, prim-compat, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "prim-bool";
  version = "1.0.1";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-bool";
    sha256 = "0ww181n143p829xb99mbi7wnjd1d5rps7z4r3av78g1khma4164i";
    rev = "c293526c20420d3e428179d3da176dc5b1ad8904";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base ghc-prim prim-compat template-haskell
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/prim-bool";
  description = "Unboxed booleans";
  license = lib.licenses.isc;
}
