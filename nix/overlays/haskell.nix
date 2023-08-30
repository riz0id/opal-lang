final: prev:

{
  haskellPackages = prev.haskell.packages.ghc962.override {
    overrides = (new: old: {
      eff = old.callPackage ../pkgs/eff.nix { };

      # language-haskell-extract = old.callPackage ../pkgs/language-haskell-extract.nix { };

      opal = old.callCabal2nix "opal" ../../packages/opal { };
    });
  };
}