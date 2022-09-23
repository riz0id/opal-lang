{ ghc }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (self: _: {
        source-locations = self.callPackage ../pkgs/source-locations.nix { };
      });
    };
  };
}