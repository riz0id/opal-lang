{ ghc }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (self: _: {
        prim-char = self.callPackage ../pkgs/prim-char.nix { };
      });
    };
  };
}