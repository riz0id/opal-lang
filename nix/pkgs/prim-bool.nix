{ mkDerivation, base, fetchgit, ghc-prim, hedgehog, lib
, prim-compat, tasty, tasty-hedgehog, template-haskell
}:
mkDerivation {
  pname = "prim-bool";
  version = "1.0.1";
  src = fetchgit {
    url = "https://github.com/riz0id/prim-bool";
    sha256 = "06n0j9ya0ckfmqklp26m22p1j59l3k33ksmcfrcwkgdj7s1jv5qa";
    rev = "7e230c5a1a6a1b34848defc6172f38607c912b14";
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
  mainProgram = "example";
}
