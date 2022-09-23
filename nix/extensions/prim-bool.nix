{ ghc }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (self: _: {
        prim-bool = self.callPackage ../pkgs/prim-bool.nix { };
      });
    };
  };
}