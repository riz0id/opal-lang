
with import nix/pkgs.nix;

pkgs.pkgs.haskell.packages.ghc962.opal.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ (with pkgs; [
    cabal-install
    haskell.compiler.ghc962
    haskell.packages.ghc962.haskell-language-server
  ]);
})