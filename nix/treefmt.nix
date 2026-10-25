{ inputs, ... }:
{
  imports = [ inputs.treefmt-nix.flakeModule ];
  perSystem = _: {
    treefmt = {
      projectRootFile = "flake.nix";

      programs.nixfmt.enable = true;
      programs.just.enable = true;
      programs.mdformat.enable = true;
      programs.yamlfmt = {
        enable = true;
        settings.formatter.retain_line_breaks_single = true;
      };

      # No Scheme formatter: treefmt-nix has none, and the nixpkgs candidate formats
      # S-expressions without knowing Guile, splitting every #:use-module from its
      # module name. Scheme is checked instead, by `just lint` and `just whitespace`.
    };
  };
}
