{ mkDerivation, base, fetchgit, ghc-prim, lib, prim-bool, prim-char
, prim-int, template-haskell
}:
mkDerivation {
  pname = "source-locations";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/source-locations";
    sha256 = "0jldmykczxpmbd0j75cwv6rjv58942n864yda166kiywdh15ky2v";
    rev = "b0c50f599a99dc5d7f5d4de58b78d8e357790531";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim prim-bool prim-char prim-int template-haskell
  ];
  homepage = "https://github.com/riz0id/source-locations";
  description = "Create and manipulate source file locations";
  license = lib.licenses.isc;
}
