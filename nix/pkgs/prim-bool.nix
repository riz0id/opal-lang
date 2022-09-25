{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib
, prim-compat, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "prim-bool";
  version = "1.0.1";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-bool";
    sha256 = "0zq7qf1rnh9azvx59646yvg37vfb177g1z6fyja0l1g9j7xd1gdg";
    rev = "3d8c5b2276915df7ab7886af99caa6d162c53206";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base ghc-prim prim-compat template-haskell
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/prim-bool";
  description = "Unboxed booleans";
  license = lib.licenses.isc;
}
