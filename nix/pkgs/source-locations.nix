{ mkDerivation, base, deepseq, fetchgit, ghc-prim, hedgehog, lib
, prim-bool, prim-char, prim-int, prim-ord, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "source-locations";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/source-locations";
    sha256 = "1g5x34j3xngbbp5ni9z0n57gzywr7rc67kswrasy2dkrx9ahqwkg";
    rev = "0ef56e51d942de7e0f3e2be49704c14b4d31c4a6";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base deepseq ghc-prim prim-bool prim-char prim-int prim-ord
    template-haskell
  ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/source-locations";
  description = "Create and manipulate source file locations";
  license = lib.licenses.isc;
}
