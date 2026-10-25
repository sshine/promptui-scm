{ inputs, ... }:
{
  imports = [ inputs.hk-nix.flakeModules.default ];
  perSystem =
    {
      config,
      pkgs,
      lib,
      ...
    }:
    let
      # Tools by absolute store path: the `nix flake check` hk-check sandbox runs hooks
      # without the devshell PATH.
      treefmt = lib.getExe config.treefmt.build.wrapper;
      guile = pkgs.guile_3_0;

      # Guile's compiler is the linter: it resolves every toplevel and checks arities
      # and format strings. guild exits 0 on a warning, so the output is the signal.
      guild-lint = pkgs.writeShellScript "promptui-guild-lint" ''
        export GUILE_AUTO_COMPILE=0
        # guild writes atomically via mkstemp, so the output has to be a real file.
        tmp=$(mktemp -d)
        trap 'rm -rf "$tmp"' EXIT
        status=0
        for f in "$@"; do
          case "$f" in src/*) ;; *) continue ;; esac
          out=$(${guile}/bin/guild compile -L "$PWD/src" \
            -Wunbound-variable -Wmacro-use-before-definition \
            -Warity-mismatch -Wformat \
            -o "$tmp/$(echo "$f" | tr / _).go" "$f" 2>&1 >/dev/null)
          if [ -n "$out" ]; then echo "$out"; status=1; fi
        done
        exit $status
      '';

      whitespace = pkgs.writeShellScript "promptui-whitespace" ''
        status=0
        for f in "$@"; do
          case "$f" in *.scm) ;; *) continue ;; esac
          if ${pkgs.gnugrep}/bin/grep -nP '\t' "$f"; then
            echo "  ^ tab in $f"; status=1
          fi
          if ${pkgs.gnugrep}/bin/grep -nP ' +$' "$f"; then
            echo "  ^ trailing space in $f"; status=1
          fi
          if [ -s "$f" ] && [ -n "$(tail -c1 "$f")" ]; then
            echo "$f: no newline at end of file"; status=1
          fi
        done
        exit $status
      '';

      run-tests = pkgs.writeShellScript "promptui-run-tests" ''
        export GUILE_AUTO_COMPILE=0
        export PATH=${guile}/bin:$PATH
        exec guile -L "$PWD/src" -L "$PWD/tests" -s "$PWD/tests/run-tests.scm"
      '';
    in
    {
      hk-nix.settings.hooks = {
        "pre-commit" = {
          fix = true;
          stash = "git";
          steps = {
            treefmt.check = "${treefmt} --fail-on-change --no-cache {{files}}";
            treefmt.fix = "${treefmt} {{files}}";
            whitespace.check = "${whitespace} {{files}}";
          };
        };

        "pre-push".steps = {
          deadnix.glob = "*.nix";
          deadnix.check = "${lib.getExe pkgs.deadnix} --fail {{files}}";
          guild.glob = "*.scm";
          guild.check = "${guild-lint} {{files}}";
          test.check = "${run-tests}";
        };

        "commit-msg".steps.conventional.builtin = config.hk-nix.builtins.check_conventional_commit;
      };
    };
}
