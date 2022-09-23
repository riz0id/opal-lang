{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, prim-bool
, prim-compat, prim-int, primitive, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "prim-bytes";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-bytes";
    sha256 = "1g7rggd9dgs6hllvchrmdjl54fqm57l8c1jgp90hzyjn1dmmb1jr";
    rev = "c4b359b10ce2771de510fb544c094a9397208fb5";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim prim-bool prim-compat prim-int primitive
    template-haskell
  ];
  testHaskellDepends = [
    base hedgehog primitive tasty tasty-hedgehog
  ];
  homepage = "https://github.com/riz0id/prim-bytes";
  description = "TODO";
  license = lib.licenses.isc;
}
