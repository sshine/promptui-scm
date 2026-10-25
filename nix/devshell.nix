{ inputs, ... }:
{
  imports = [ inputs.devshell.flakeModule ];
  perSystem =
    { config, pkgs, ... }:
    {
      devshells.default = {
        packages = [
          pkgs.guile_3_0
          config.treefmt.build.wrapper
          config.hk-nix.package
          pkgs.deadnix
          pkgs.git
          pkgs.just
        ];

        env = [
          # $PRJ_ROOT/src first, so the working tree shadows any installed copy.
          {
            name = "GUILE_LOAD_PATH";
            prefix = "$PRJ_ROOT/src";
          }
          # Interpreted rather than compiled, so an edited module is what runs.
          {
            name = "GUILE_AUTO_COMPILE";
            value = "0";
          }
        ];

        devshell.motd = "";
        devshell.startup.hk.text = config.hk-nix.shellHook;
      };
    };
}
