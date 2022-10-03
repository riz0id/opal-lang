{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, prim-bool
, prim-compat, prim-int, prim-ord, primitive, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "prim-bytearray";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-bytearray";
    sha256 = "0xazk1hgbdn94p2h8dv036270nx60kpzm8ppmi1fciawbwva4jq9";
    rev = "20a5fe4781181fa491855d8e4f0120c53478f6d2";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim prim-bool prim-compat prim-int prim-ord primitive
    template-haskell
  ];
  testHaskellDepends = [
    base hedgehog prim-bool prim-ord primitive tasty tasty-hedgehog
  ];
  homepage = "https://github.com/riz0id/prim-bytearray";
  description = "TODO";
  license = lib.licenses.isc;
}
