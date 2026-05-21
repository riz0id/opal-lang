final: prev:

let
  inherit (prev) haskell;
  inherit (haskell.lib.compose) overrideCabal;

in {
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      ghc96 = prev.haskell.packages.ghc96.extend (new: old: {
        eff = old.callPackage ../../pkgs/eff.nix { };

        opal = old.callCabal2nix "opal" ../../../packages/opal { };
      });
    };
  };
}