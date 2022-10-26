{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib, tasty
, tasty-hedgehog
}:
mkDerivation {
  pname = "prim-compat";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-compat";
    sha256 = "1fwx6295i518vjbrm4mvyh0scp80zbw4nwzr4kjjs16p79n7hi9y";
    rev = "9b15172db77376ce2dc31ab6b45e9e4bf75aef3e";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base ghc-prim ];
  testHaskellDepends = [ base hedgehog tasty tasty-hedgehog ];
  homepage = "https://github.com/riz0id/prim-compat";
  description = "Lightweight ghc-prim wrappers for backwards compatibility";
  license = lib.licenses.isc;
}
