{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, mtl
, prim-bool, prim-compat, prim-int, prim-ord, primitive
, source-locations, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "parsel";
  version = "0.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/parsel";
    sha256 = "0sgfdw39if8kczwwgw7yzpwa6n3wfpbijdzb73aw329q3hbsqfxk";
    rev = "bbb36afd66c948d76916cbc7d5aa93428cd336f5";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim mtl prim-bool prim-compat prim-int prim-ord primitive
    source-locations template-haskell
  ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/parsel";
  description = "TODO";
  license = lib.licenses.isc;
}
