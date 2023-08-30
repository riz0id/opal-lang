
with import nix/pkgs.nix;

mkShell {
  buildInputs = [
    haskellPackages.eff
  ];
}
# pkgs.haskellPackages.opal.env.overrideAttrs (self: {
#   buildInputs = self.buildInputs ++ (with pkgs; [
#     # haskellPackages.ghc962
#     # haskellPackages.haskell-language-server
#   ]);
# })