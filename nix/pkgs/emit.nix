{ mkDerivation, base, fetchgit, ghc-prim, lib, mtl, prim-bool
, prim-char, prim-int, prim-ord, primitive, template-haskell, text
}:
mkDerivation {
  pname = "emit";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/emit";
    sha256 = "12p2yc9254br95xirzf9z386gliwmkq6m1rp6ad9awgh0435425w";
    rev = "abbc700bc2dceb77c5545656cb53f420cc1ce5b0";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim mtl prim-bool prim-char prim-int prim-ord primitive
    template-haskell text
  ];
  homepage = "https://github.com/riz0id/emit";
  description = "TODO";
  license = lib.licenses.isc;
}
