final: prev: 

let 
  # Obtain the GHC version from config.
  ghc = final.config.ghc;

  compiler = prev.haskell.compiler."${ghc}";
  packages = prev.haskell.packages."${ghc}";
in {
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (_: prev: {
        hlint = prev.callHackage "hlint" "3.5" { };

      });
    };
  };
}