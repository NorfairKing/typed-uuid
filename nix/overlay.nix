final:
  previous:
    with final.haskell.lib;
    {
      typedUuidPackages = 
            let pkg = name:
                (failOnAllWarnings (final.haskellPackages.callCabal2nix name (../. + "/${name}") {}));
            in final.lib.genAttrs [
              "typed-uuid"
              "genvalidity-typed-uuid"
            ] pkg;
      haskellPackages = previous.haskellPackages.override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (
          self: super: final.typedUuidPackages
        );
      });
    }
