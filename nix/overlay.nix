final:
previous:
with final.haskell.lib;
{
  typedUuidPackages =
    let
      pkg = name: buildStrictly (final.haskellPackages.callCabal2nixWithOptions name (final.gitignoreSource (../. + "/${name}")) "--no-hpack" {});
    in
      final.lib.genAttrs [
        "typed-uuid"
        "genvalidity-typed-uuid"
      ] pkg;
  haskellPackages = previous.haskellPackages.override (
    old: {
      overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (
        self: super: final.typedUuidPackages
      );
    }
  );
}
