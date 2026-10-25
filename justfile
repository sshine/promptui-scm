# Default recipe: list available commands
default:
    @just --list

# Format all code (Nix + Markdown + YAML + justfile)
fmt:
    treefmt

# Check formatting
fmt-check:
    treefmt --fail-on-change --no-cache

# Lint every module with the Guile compiler
lint:
    #!/usr/bin/env bash
    set -uo pipefail
    # guild writes atomically via mkstemp, so the output has to be a real file.
    tmp=$(mktemp -d)
    trap 'rm -rf "$tmp"' EXIT
    status=0
    while IFS= read -r f; do
        # guild exits 0 on a warning, so the diagnostics themselves are the signal.
        out=$(guild compile -L src \
            -Wunbound-variable -Wmacro-use-before-definition \
            -Warity-mismatch -Wformat \
            -o "$tmp/$(echo "${f#src/}" | tr / _)o" "$f" 2>&1 >/dev/null)
        if [ -n "$out" ]; then echo "$out"; status=1; fi
    done < <(find src -name '*.scm')
    exit $status

# Check whitespace hygiene in Scheme sources
whitespace:
    #!/usr/bin/env bash
    set -uo pipefail
    status=0
    while IFS= read -r f; do
        if grep -nP '\t' "$f"; then echo "  ^ tab in $f"; status=1; fi
        if grep -nP ' +$' "$f"; then echo "  ^ trailing space in $f"; status=1; fi
        if [ -s "$f" ] && [ -n "$(tail -c1 "$f")" ]; then
            echo "$f: no newline at end of file"; status=1
        fi
    done < <(find src tests examples -name '*.scm')
    exit $status

# Run the test suite
test:
    guile -L src -L tests -s tests/run-tests.scm

# Run a single suite, e.g. `just test-one widgets`
test-one NAME:
    guile -L src -L tests -s tests/{{ NAME }}-test.scm

# Compile every module to bytecode under build/
build:
    #!/usr/bin/env bash
    set -euo pipefail
    while IFS= read -r f; do
        out="build/ccache/${f#src/}"
        guild compile -L src -o "${out%.scm}.go" "$f"
    done < <(find src -name '*.scm')

# Run the chat example
demo:
    guile -L src examples/chat.scm

# Run CI checks locally
ci: fmt-check whitespace lint test build
    @echo "All CI checks passed!"

# Clean build artifacts
clean:
    rm -rf build
    find src tests -name '*.go' -delete
