{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, prim-bool
, prim-char, prim-int, prim-ord, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "source-locations";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/source-locations";
    sha256 = "03h5f62m920xis2skp40ib2bvhk4qygf84a2xyw1b25jh0aim2mp";
    rev = "45546d124129f2f93a143d8f709b8bf19dcb8074";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim prim-bool prim-char prim-int prim-ord
    template-haskell
  ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/source-locations";
  description = "Create and manipulate source file locations";
  license = lib.licenses.isc;
}
