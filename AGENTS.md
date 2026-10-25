# Working on promptui-scm

## No C

The library is pure Guile. Do not reach for `(system foreign)`, a C extension, or a
binding to ncurses, newt or slang. Terminal state goes through `stty`, the size through
`stty size`, and everything else is escape sequences written to a port.

## Width is the invariant

A cell buffer is only correct if every string is exactly as wide as it claims. Measure
with `string-width`, never `string-length`, and write text with `buffer-set-string!`,
which drops control characters and spans wide characters across two cells.

## Measuring and drawing agree

Every widget that reports a height uses `(promptui wrap)` and `(promptui text)` to
compute it, and draws with the same procedures. A layout built from a measurement must
match what is then drawn into it.

## Conventions

- One module per file, the name matching the path: `(promptui widgets text-field)` is
  `src/promptui/widgets/text-field.scm`.
- A widget is a record, a `make-…` constructor taking the theme first and options as
  keywords, and a `…-render!` taking the widget, an area and a buffer, in that order.
- Anything that animates takes the time in milliseconds as an optional last argument
  defaulting to `now-ms`, so a test can step it exactly.
- Hex escapes in strings are `\xHH` with exactly two digits and no semicolon. Guile's
  reader is not in R6RS mode, so `"\x1b;[0m"` is not an escape sequence.

## Commands

```sh
just test        # the suite
just lint        # the Guile compiler, as a linter
just ci          # what CI runs
```
