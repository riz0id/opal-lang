{ mkDerivation, base, fetchgit, ghc-prim, lib, prim-bool, prim-char
, prim-compat, prim-int, primitive, template-haskell
}:
mkDerivation {
  pname = "buffers";
  version = "1.0.1";
  src = fetchgit {
    url = "https://github.com/riz0id/buffers";
    sha256 = "1cbp2khp4sf17hfqhslrfmzs1c7dfs6iwbyc6bfixry7jqwp6mwn";
    rev = "bda37a5e4f962640e159c5524036bc9d9ec31d81";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base ghc-prim prim-bool prim-char prim-compat prim-int primitive
    template-haskell
  ];
  homepage = "https://github.com/riz0id/buffers";
  description = "TODO";
  license = lib.licenses.isc;
}
