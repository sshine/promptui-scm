# promptui-scm

TUI widgets for Guile Scheme in the style of OpenCode's CLI, ported from
[promptui-rs](../promptui-rs). Pure Scheme: no C bindings, no FFI, no ncurses.

promptui-rs is built on [ratatui](https://ratatui.rs), so this library carries its own small
equivalent of it underneath the widgets.

## Layers

The core does what ratatui and crossterm do for promptui-rs.

| Module | Purpose |
| --- | --- |
| `(promptui width)` | Display width of characters and strings, wide and combining included |
| `(promptui style)` | Colours (ANSI, 256, 24-bit, terminal default), modifiers, SGR |
| `(promptui rect)` | Rectangles and constraint layout |
| `(promptui buffer)` | The cell grid a frame is drawn into |
| `(promptui wrap)` | Word wrapping by display width |
| `(promptui text)` | Styled spans and lines, and paragraphs |
| `(promptui input)` | Key decoding: legacy sequences, kitty CSI u, bracketed paste |
| `(promptui terminal)` | Raw mode, size, frame diffing, reading keys |
| `(promptui app)` | The event loop: draw, wait on keys, ticks and ports, react |
| `(promptui theme)` | The semantic palette: OpenCode's, or the terminal's own |
| `(promptui padding)` | Insets |
| `(promptui focus)` | The focus stack |

The widgets live under `(promptui widgets …)`.

| Widget | Purpose |
| --- | --- |
| `agent-overview` | Summary panel for running agents, in three sizes |
| `agent-scope` | Header line for one agent in a tree |
| `border-box` | Left-bar, square or rounded frame |
| `breadcrumb` | Path-style navigation trail |
| `message-list` | Chat transcript of messages, tool calls and plans |
| `notice` | Short-lived notification with a countdown |
| `questionnaire` | Multi-question form with a summary page |
| `scroll-box` | Bottom-anchored viewport with scroll state |
| `scrollbar` | Half-cell precision scrollbar |
| `spinner` | Activity indicator |
| `status-bar` | Mode, activity, model, directory, latency and context |
| `streaming-text` | Character-by-character text reveal |
| `text-field` | Multi-line wrapping input |
| `todo-list` | Checklist with progress |
| `truncated-block` | Height-capped block with an overflow hint |

## Use

A widget is a record and a `…-render!` procedure that draws it into an area of a buffer:

```scheme
(use-modules (promptui terminal) (promptui app) (promptui theme)
             (promptui widgets message-list))

(define ml (make-message-list %terminal-theme))
(message-list-push-message! ml 'assistant "hello")

(run-app (make-terminal)
         #:draw (lambda (frame)
                  (message-list-render! ml (frame-area frame) (frame-buffer frame)))
         #:on-key (lambda (key) (set! done #t))
         #:running? (lambda () (not done)))
```

`examples/chat.scm` puts most of the widgets together.

## Development

```sh
nix develop      # or `direnv allow`
just --list
just test
just demo
```

## License

MIT OR Apache-2.0.
