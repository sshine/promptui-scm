# The build definition, shared by this flake's packages and by the overlay.
{
  lib,
  stdenv,
  guile_3_0,
}:
stdenv.mkDerivation {
  pname = "guile-promptui";
  version = "0.1.0";

  src = lib.fileset.toSource {
    root = ../.;
    fileset = lib.fileset.unions [
      ../src
      ../tests
      ../examples
      ../README.md
    ];
  };

  nativeBuildInputs = [ guile_3_0 ];
  buildInputs = [ guile_3_0 ];

  buildPhase = ''
    runHook preBuild
    find src -name '*.scm' -print0 | while IFS= read -r -d "" f; do
      guild compile -L src -Wunbound-variable -Warity-mismatch -Wformat \
        -o "''${f%.scm}.go" "$f"
    done
    runHook postBuild
  '';

  # The suite runs in the sandbox too, which has no terminal and the C locale. That
  # is where a width table that leaned on the locale would show itself.
  # Guile bytecode is ELF, but there is nothing in it for patchelf or strip to fix.
  dontPatchELF = true;
  dontStrip = true;

  doCheck = true;
  checkPhase = ''
    runHook preCheck
    guile -L src -L tests -s tests/run-tests.scm
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/guile/site/3.0 $out/lib/guile/3.0/site-ccache
    (cd src && find . -name '*.scm' -exec install -Dm644 {} "$out/share/guile/site/3.0/{}" \;)
    (cd src && find . -name '*.go' -exec install -Dm644 {} "$out/lib/guile/3.0/site-ccache/{}" \;)
    install -Dm644 examples/chat.scm $out/share/doc/guile-promptui/examples/chat.scm
    runHook postInstall
  '';

  meta = {
    description = "OpenCode-style TUI widgets for Guile Scheme, with no C bindings";
    license = with lib.licenses; [
      mit
      asl20
    ];
    platforms = guile_3_0.meta.platforms;
  };
}
