let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv {};
  validity-overlay = import (pkgs.fetchFromGitHub (import ./validity-version.nix) + "/nix/overlay.nix");
  yamlparse-applicative-overlay = import (pkgs.fetchFromGitHub (import ./yamlparse-applicative-version.nix) + "/nix/overlay.nix");
  typedUuidPkgs = pkgsv {
    overlays = [
      validity-overlay
      yamlparse-applicative-overlay
      (import ./gitignore-src.nix)
      (import ./overlay.nix)
    ];
    config.allowUnfree = true;
  };
in typedUuidPkgs
