{ mkDerivation, base, criterion, deepseq, fetchgit, ghc-prim
, hedgehog, lib, mtl, prim-bool, prim-compat, prim-int, prim-ord
, primitive, source-locations, tasty, tasty-hedgehog
, template-haskell, text
}:
mkDerivation {
  pname = "parsel";
  version = "0.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/parsel";
    sha256 = "1mhay40gw4vk5lmrr0k5lzb2ai91rhgvp79fixvlkibmhksdlhx6";
    rev = "e84029d808676ea229b24b0a81e501f34e666083";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base deepseq ghc-prim mtl prim-bool prim-compat prim-int prim-ord
    primitive source-locations template-haskell text
  ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog text ];
  benchmarkHaskellDepends = [
    base criterion deepseq primitive text
  ];
  homepage = "https://github.com/riz0id/parsel";
  description = "TODO";
  license = lib.licenses.isc;
}
