{ mkDerivation, base, fetchgit, ghc-prim, lib, prim-bool
, prim-compat, prim-int, template-haskell
}:
mkDerivation {
  pname = "prim-char";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-char";
    sha256 = "1xcl7xwkrh2nx9ra82llkv29f99ja90573vi1yn224j2jbn0gp3x";
    rev = "68907f4bcf87c0d200531abaf4c07219b71077fb";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim prim-bool prim-compat prim-int template-haskell
  ];
  homepage = "https://github.com/riz0id/prim-char";
  description = "Facilities for working with unboxed characters";
  license = lib.licenses.isc;
}
