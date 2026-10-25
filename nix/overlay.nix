_: {
  flake.overlays.default = final: _prev: {
    guile-promptui = final.callPackage ./_package.nix { };
  };
}
