{ mkDerivation, base, fetchgit, hspec, hspec-discover, lib
, primitive
}:
mkDerivation {
  pname = "eff";
  version = "0.0.0.0";
  src = fetchgit {
    url = "https://github.com/riz0id/eff";
    sha256 = "00g8jq2pna8vs82cz01jcp7d9dky8b3cr6yrk4fiiim90w876hl4";
    rev = "add09e5af6ee614ffc1fa37c48476210f5557c99";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/eff; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base primitive ];
  testHaskellDepends = [ base hspec primitive ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/hasura/eff";
  license = lib.licenses.isc;
}
