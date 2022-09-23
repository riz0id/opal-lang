{ ghc ? "ghc922" }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs.haskell.packages."${ghc}") 
    fourmolu
    haskell-language-server
    hlint
    opal-lang
    opal-repl;
    
  inherit (pkgs) 
    cabal-install 
    clang 
    llvm;

  ncurses = pkgs.ncurses6;
}