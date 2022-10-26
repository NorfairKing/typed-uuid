final: prev:
with final.lib;
with final.haskell.lib;
{
  typedUuidRelease = final.symlinkJoin {
    name = "typed-uuid-release";
    paths = attrValues final.haskellPackages.typedUuidPackages;
  };
  haskellPackages = prev.haskellPackages.override (
    old: {
      overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
        self: super:
          let
            typedUuidPackages =
              let
                pkg = name:
                  buildStrictly (self.callPackage (../${name}/default.nix) { });
              in
              genAttrs [
                "typed-uuid"
                "genvalidity-typed-uuid"
              ]
                pkg;
          in
          {
            inherit typedUuidPackages;
          } // typedUuidPackages
      );
    }
  );
}
