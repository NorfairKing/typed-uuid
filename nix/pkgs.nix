{ sources ? import ./sources.nix
}:
let
  pkgsv = import sources.nixpkgs;
in
import sources.nixpkgs {
  overlays = [
    (import (sources.validity + "/nix/overlay.nix"))
    (import (sources.autodocodec + "/nix/overlay.nix"))
    (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
    (import ./gitignore-src.nix)
    (import ./overlay.nix)
  ];
  config.allowUnfree = true;
}
