import <nixpkgs> {
  overlays = [
    (import overlays/haskell/packages.nix)
  ];
}