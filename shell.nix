
with import nix/pkgs.nix;

mkShell {
  buildInputs = [
    cabal-install
    haskell.compiler.ghc962
    haskell.packages.ghc962.haskell-language-server
  ];
}