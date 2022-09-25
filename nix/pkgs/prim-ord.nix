{ mkDerivation, base, fetchgit, ghc-prim, lib, prim-bool
, prim-compat, template-haskell
}:
mkDerivation {
  pname = "prim-ord";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-ord";
    sha256 = "1iq1kn98fxgpwcr4g4cs44isa23rxi70jlmbykmxh24i17q6bagg";
    rev = "bf916955d72102ae80402053bd098d58c854710f";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim prim-bool prim-compat template-haskell
  ];
  homepage = "https://github.com/riz0id/prim-ord";
  description = "TODO";
  license = lib.licenses.isc;
}
