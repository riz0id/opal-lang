{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, prim-bool
, prim-compat, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "prim-int";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-int";
    sha256 = "0csbscqiz9yfsf76mhgq9fi24igcb2ps0dy78rvhyal9n20gqag0";
    rev = "b752f3fbb35f8a51dab7a3c0d75f9fb7a2b9710b";
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
