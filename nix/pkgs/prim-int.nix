{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, prim-bool
, prim-compat, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "prim-int";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-int";
    sha256 = "0l3x86sxmnsgkpk4nyf3ph2yfk64nl40h4dixhn8ibcaxlslj3vb";
    rev = "65227f5a7bc0f0a183eecf5aaf08d76b708c3f64";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim prim-bool prim-compat template-haskell
  ];
  testHaskellDepends = [
    base ghc-prim hedgehog prim-bool prim-compat tasty tasty-hedgehog
  ];
  homepage = "https://github.com/riz0id/prim-int";
  description = "Facilities for working with unlifted integers";
  license = lib.licenses.isc;
}
