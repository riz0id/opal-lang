{ ghc }:

let
  nixpkgs = import ./nixpkgs.nix { };
in import nixpkgs {
  config.packageOverrides = pkgs: 
    pkgs.lib.composeManyExtensions [  
      (import extensions/opal.nix {
        inherit ghc;
      })
      (import extensions/parsel.nix {
        inherit ghc;
      })
      (import extensions/prim-bool.nix {
        inherit ghc;
      })
      (import extensions/prim-bytearray.nix {
        inherit ghc;
      })
      (import extensions/prim-char.nix {
        inherit ghc;
      })
      (import extensions/prim-compat.nix {
        inherit ghc;
      })
      (import extensions/prim-int.nix {
        inherit ghc;
      })
      (import extensions/prim-ord.nix {
        inherit ghc;
      })
      (import extensions/source-locations.nix {
        inherit ghc;
      })
    ] pkgs pkgs;
}