final: prev:

let
  # Obtain the GHC version from config.
  ghc = final.config.ghc;
in {
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (_: old: {
        hlint = old.callHackage "hlint" "3.5" { };
        opal = old.callCabal2nix "opal" ../../packages/opal { };
      });
    };
  };
}