{ ghc }:

let
  pkgs = import nix/pkgs.nix {
    inherit ghc;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}")
    haskell-language-server
    opal
    stylish-haskell;
}