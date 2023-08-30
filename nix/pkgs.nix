import <nixpkgs> {
  overlays = [
    (import overlays/haskell.nix)
  ];
}