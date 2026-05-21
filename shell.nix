{ pkgs ? import nix/pkgs.nix }:

let
  hpkgs = pkgs.haskell.packages.ghc96;
in hpkgs.shellFor {
  packages = hpkgs: [ hpkgs.opal ];

  nativeBuildInputs = [
    pkgs.haskell.compiler.ghc96
    hpkgs.cabal-install
    hpkgs.haskell-language-server
    hpkgs.ghcid
    pkgs.hlint
  ];
}
