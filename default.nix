{ ghc ? "ghc924" }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") 
    opal-lang
    opal-repl;
    
  inherit (pkgs) 
    clang 
    llvm;

  ncurses = pkgs.ncurses6;
}