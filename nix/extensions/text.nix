{ ghc }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (self: _: {
        text = if ghc == "ghc924" 
          then prev.haskell.packages."${ghc}".text_2_0_1
          else prev.haskell.packages."${ghc}".text;
      });
    };
  };
}