{ mkDerivation, base, deepseq, fetchgit, ghc-prim, hedgehog, lib
, prim-bool, prim-char, prim-int, prim-ord, tasty, tasty-hedgehog
, template-haskell
}:
mkDerivation {
  pname = "source-locations";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/source-locations";
    sha256 = "0xdkx752vhl6l6fwpq7jswpxrrs2yk4frz823jxmdwccah0g0lhc";
    rev = "7380664b8a627810c70949bf8741a3e501e59f5c";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base deepseq ghc-prim prim-bool prim-char prim-int prim-ord
    template-haskell
  ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/source-locations";
  description = "Create and manipulate source file locations";
  license = lib.licenses.isc;
}
