{ mkDerivation, base, fetchgit, ghc-prim, lib, prim-bool
, prim-compat, template-haskell
}:
mkDerivation {
  pname = "prim-ord";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-ord";
    sha256 = "11qr5d51kb5gffji9iplwlwyznk7sp2zw27jiqjlwv86f4gwlwih";
    rev = "15a775b6d76830e44aea980df82c45dca0bb6104";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim prim-bool prim-compat template-haskell
  ];
  homepage = "https://github.com/riz0id/prim-ord";
  description = "TODO";
  license = lib.licenses.isc;
}
