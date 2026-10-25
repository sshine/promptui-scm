_: {
  perSystem =
    { pkgs, ... }:
    rec {
      checks.guile-promptui = packages.default;
      packages.default = pkgs.callPackage ./_package.nix { };
    };
}
