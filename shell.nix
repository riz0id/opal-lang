{ ghc ? "ghc922" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.opal-lang.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ [ 
    pkgs.cabal-install
    pkgs.clang
    pkgs.fourmolu
    pkgs.hlint
    pkgs.haskell-language-server
    pkgs.llvm
  ];
})