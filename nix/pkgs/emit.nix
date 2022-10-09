{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, mtl
, prim-bool, prim-char, prim-int, prim-ord, primitive, tasty
, tasty-hedgehog, template-haskell, text
}:
mkDerivation {
  pname = "emit";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/emit";
    sha256 = "0nmbzbb7wn7f5q9l6bdp3rhl9w66k39vq1ay9lfsgc00dj8sl69n";
    rev = "c214d6606274436e815abf3ce2cdd1113480b9ff";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim mtl prim-bool prim-char prim-int prim-ord primitive
    template-haskell text
  ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/emit";
  description = "TODO";
  license = lib.licenses.isc;
}
