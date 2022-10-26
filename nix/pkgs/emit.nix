{ mkDerivation, base, fetchgit, ghc-prim, lib, mtl, primitive
, template-haskell, text
}:
mkDerivation {
  pname = "emit";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/emit";
    sha256 = "1wnibk77qxm17wf5jlw3z20cvsgdkdgbchrl9k076zhkhvdj6gyv";
    rev = "3dfaca543e241a049717c90d617425420466576c";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim mtl primitive template-haskell text
  ];
  homepage = "https://github.com/riz0id/emit";
  description = "TODO";
  license = lib.licenses.isc;
}
