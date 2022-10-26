{ ghc ? "ghc924" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.opal-lang.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ [ 
    pkgs.clang
    pkgs.llvm
  ];
})