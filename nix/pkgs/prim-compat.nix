{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, tasty
, tasty-hedgehog
}:
mkDerivation {
  pname = "prim-compat";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-compat";
    sha256 = "1chjxab9pm1x3rm384jcs0zavxlp34bz56fcpc4p8xh10ggv25xc";
    rev = "1bfd13c3ce8425a6fb0ddb71986861d0d9724863";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base ghc-prim ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/prim-compat";
  description = "Lightweight ghc-prim wrappers for backwards compatibility";
  license = lib.licenses.isc;
}
