final: prev:

with prev.lib;

{
  haskellPackages = concatMapAttrs
    (name: value: {
      ${name} = value.overrideAttrs (old: old // {
        enableLibraryProfiling = false;
      });
    })
    prev.haskellPackages;
}