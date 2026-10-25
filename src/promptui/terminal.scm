;;; The terminal: raw mode, size, drawing a buffer, and reading keys.
;;;
;;; Everything here is ordinary Scheme over ports and subprocesses. Raw mode and the
;;; window size come from stty rather than a binding to tcsetattr and ioctl: struct
;;; termios is laid out differently on Linux and Darwin, so a hand-written binding is
;;; a standing portability liability for something a fork does correctly on both.
;;; The cost is a process per query, so the size is cached and asked again only when
;;; the window changes.
;;;
;;; Drawing follows ratatui. A frame is rendered into a fresh buffer, compared with
;;; the previous one, and only the rows that changed are rewritten, using absolute
;;; positioning on the alternate screen inside a synchronized-output pair.

(define-module (promptui terminal)
  #:use-module (promptui width)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (promptui input)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:export (stty-size
            raw-mode-on!
            raw-mode-off!
            make-terminal
            terminal?
            terminal-input
            terminal-output
            terminal-size
            terminal-area
            terminal-refresh-size!
            terminal-enter!
            terminal-leave!
            terminal-active?
            terminal-mouse?
            terminal-set-mouse!
            terminal-invalidate!
            terminal-draw!
            terminal-read-keys
            frame?
            frame-area
            frame-buffer
            frame-set-cursor!
            frame-cursor
            render-diff))

;;; Raw mode and size

(define (stty-capture . args)
  "Run stty with ARGS against standard input, returning what it printed or #f.
Through a shell so that its complaint about a standard input that is not a terminal
goes nowhere rather than onto the screen being drawn."
  (false-if-exception
   (let* ((port (open-input-pipe
                 (string-append "stty " (string-join args " ") " 2>/dev/null")))
          (out (get-string-all port)))
     (and (zero? (status:exit-val (close-pipe port)))
          (string-trim-both out)))))

(define (stty-size)
  "(ROWS . COLUMNS) as stty reports them for standard input, or #f."
  (let ((out (stty-capture "size")))
    (and out
         (let ((nums (filter-map string->number (string-split out #\space))))
           (and (= 2 (length nums))
                (> (car nums) 0) (> (cadr nums) 0)
                (cons (car nums) (cadr nums)))))))

(define (env-size)
  (let ((rows (and=> (getenv "LINES") string->number))
        (cols (and=> (getenv "COLUMNS") string->number)))
    (and rows cols (> rows 0) (> cols 0) (cons rows cols))))

(define %saved-stty #f)

(define (raw-mode-on!)
  "Characters arrive unbuffered and unechoed, and Ctrl-C is a keystroke rather than
a signal. The previous settings are remembered so that leaving restores exactly
them rather than whatever `stty sane` thinks is sane."
  (unless %saved-stty
    (set! %saved-stty (or (stty-capture "-g") 'sane))
    (system* "stty" "raw" "-echo" "-isig"))
  #t)

(define (raw-mode-off!)
  (when %saved-stty
    (if (string? %saved-stty)
        (system* "stty" %saved-stty)
        (system* "stty" "sane"))
    (set! %saved-stty #f))
  #t)

;;; Escape sequences

(define %alt-screen-on "\x1b[?1049h")
(define %alt-screen-off "\x1b[?1049l")
(define %hide-cursor "\x1b[?25l")
(define %show-cursor "\x1b[?25h")
(define %erase-screen "\x1b[2J")
;; A cell written into the last column puts the terminal into the pending-wrap
;; state, and the next cursor movement then lands a row lower than intended. With
;; auto-wrap off nothing is ever written past the edge, which the buffer guarantees.
(define %autowrap-off "\x1b[?7l")
(define %autowrap-on "\x1b[?7h")
;; Synchronized output: the terminal presents the frame at once instead of
;; showing it half drawn, which is what flicker is.
(define %sync-begin "\x1b[?2026h")
(define %sync-end "\x1b[?2026l")
(define %paste-on "\x1b[?2004h")
(define %paste-off "\x1b[?2004l")
;; The kitty keyboard protocol, disambiguating flag only. A terminal that does not
;; know it ignores the request and keeps sending legacy sequences.
(define %kitty-push "\x1b[>1u")
(define %kitty-pop "\x1b[<u")
;; Clicks, drags and the wheel, reported in the SGR encoding: plain decimal numbers,
;; so a column past 223 is reported correctly, which the original encoding cannot do.
;; While it is on, the terminal stops selecting text itself; most terminals still
;; select with Shift held.
(define %mouse-on "\x1b[?1000h\x1b[?1002h\x1b[?1006h")
(define %mouse-off "\x1b[?1006l\x1b[?1002l\x1b[?1000l")

(define (cursor-to x y)
  (string-append "\x1b[" (number->string (+ y 1)) ";" (number->string (+ x 1)) "H"))

;;; The terminal record

(define-record-type <terminal>
  (%make-terminal input output size previous active kitty mouse pending)
  terminal?
  (input    terminal-input)
  (output   terminal-output)
  (size     %terminal-size set-terminal-size!)
  (previous terminal-previous set-terminal-previous!)
  (active   terminal-active? set-terminal-active!)
  (kitty    terminal-kitty?)
  (mouse    terminal-mouse? set-terminal-mouse!)
  (pending  terminal-pending set-terminal-pending!))

(define* (make-terminal #:key
                        (input (current-input-port))
                        (output (current-output-port))
                        (size #f)
                        (kitty-keyboard? #t)
                        (mouse? #f))
  "A terminal over INPUT and OUTPUT. SIZE, as (ROWS . COLUMNS), fixes the size
instead of asking stty, which is what a test wants. MOUSE? turns on mouse
reports, which then arrive among the keys."
  ;; The locale decides a port's encoding, and in the C locale every box-drawing
  ;; character would be written as a question mark.
  (set-port-encoding! output "UTF-8")
  (set-port-encoding! input "UTF-8")
  (let ((t (%make-terminal input output size #f #f kitty-keyboard? mouse? "")))
    (unless size (terminal-refresh-size! t))
    t))

(define (terminal-refresh-size! t)
  "Ask the terminal for its size again. Call it when the window may have changed,
for example from a SIGWINCH handler's flag."
  (set-terminal-size! t (or (stty-size) (env-size) (%terminal-size t) (cons 24 80)))
  t)

(define (terminal-size t) (%terminal-size t))

(define (terminal-area t)
  (let ((size (terminal-size t)))
    (make-rect 0 0 (cdr size) (car size))))

(define (emit t . strings)
  (for-each (lambda (s) (put-string (terminal-output t) s)) strings)
  (force-output (terminal-output t)))

(define (terminal-enter! t)
  "Take over the screen: the alternate buffer, no visible cursor until a frame
places it, no auto-wrap, bracketed paste, mouse reports if asked for and, where
available, unambiguous keys."
  (unless (terminal-active? t)
    (emit t %alt-screen-on %hide-cursor %autowrap-off %paste-on
          (if (terminal-kitty? t) %kitty-push "")
          (if (terminal-mouse? t) %mouse-on "")
          %erase-screen)
    (set-terminal-previous! t #f)
    (set-terminal-active! t #t))
  t)

(define (terminal-leave! t)
  (when (terminal-active? t)
    (emit t "\x1b[0m"
          (if (terminal-kitty? t) %kitty-pop "")
          (if (terminal-mouse? t) %mouse-off "")
          %paste-off %autowrap-on %show-cursor %alt-screen-off)
    (set-terminal-active! t #f))
  t)

(define (terminal-set-mouse! t on?)
  "Turn mouse reports on or off, at once if the terminal is in use. Off gives the
terminal back its own text selection."
  (when (and (terminal-active? t) (not (eq? (and on? #t) (terminal-mouse? t))))
    (emit t (if on? %mouse-on %mouse-off)))
  (set-terminal-mouse! t (and on? #t))
  t)

(define (terminal-invalidate! t)
  "Forget the previous frame, so the next one is drawn in full on a cleared screen."
  (set-terminal-previous! t #f)
  t)

;;; Frames

(define-record-type <frame>
  (%make-frame area buffer cursor)
  frame?
  (area   frame-area)
  (buffer frame-buffer)
  (cursor frame-cursor set-frame-cursor!))

(define (frame-set-cursor! f x y)
  "Show the terminal's cursor at X,Y once the frame is drawn."
  (set-frame-cursor! f (cons x y)))

(define (row-changed-span old new y width)
  "The first and last columns of row Y that differ, or #f when the row is unchanged.
The span starts on a whole character, never on the second half of a wide one."
  (let loop ((x 0) (first #f) (last #f))
    (if (= x width)
        (and first
             (cons (let back ((f first))
                     (if (and (> f 0) (string-null? (buffer-symbol new f y)))
                         (back (- f 1))
                         f))
                   last))
        (if (and old
                 (equal? (buffer-symbol old x y) (buffer-symbol new x y))
                 (style=? (buffer-style old x y) (buffer-style new x y)))
            (loop (+ x 1) first last)
            (loop (+ x 1) (or first x) x)))))

(define (render-diff old new)
  "The escape sequences that turn a screen showing OLD into one showing NEW. With no
OLD, every row is written."
  (let* ((area (buffer-area new))
         (width (rect-width area))
         (out (open-output-string)))
    (do ((y 0 (+ y 1))) ((= y (rect-height area)))
      (let ((span (row-changed-span old new y width)))
        (when span
          (put-string out (cursor-to (car span) y))
          (let loop ((x (car span)) (current #f))
            (when (<= x (cdr span))
              (let ((sym (buffer-symbol new x y))
                    (style (buffer-style new x y)))
                (cond
                 ((string-null? sym) (loop (+ x 1) current))
                 (else
                  (unless (and current (style=? current style))
                    (put-string out (style->sgr style)))
                  (put-string out sym)
                  (loop (+ x 1) style)))))))))
    (put-string out "\x1b[0m")
    (get-output-string out)))

(define (terminal-draw! t draw)
  "Render a frame. DRAW is called with a frame, whose buffer it fills; the rows that
differ from the previous frame are then written to the terminal."
  (terminal-enter! t)
  (let* ((area (terminal-area t))
         (buf (make-buffer area))
         (frame (%make-frame area buf #f))
         (previous (terminal-previous t))
         ;; A resize invalidates the whole previous frame: the same content sits on
         ;; different rows, and the terminal may have reflowed what it showed.
         (previous (and previous
                        (equal? (buffer-area previous) area)
                        previous)))
    (draw frame)
    (let ((cursor (frame-cursor frame)))
      (emit t
            %sync-begin
            %hide-cursor
            (if previous "" %erase-screen)
            (render-diff previous buf)
            (if cursor
                (string-append (cursor-to (car cursor) (cdr cursor)) %show-cursor)
                "")
            %sync-end))
    (set-terminal-previous! t buf)
    frame))

;;; Reading keys

(define (read-available port)
  "Whatever is buffered or readable on PORT right now, without blocking."
  (let loop ((acc '()))
    (if (not (char-ready? port))
        (list->string (reverse acc))
        (let ((ch (false-if-exception (read-char port))))
          (if (or (not ch) (eof-object? ch))
              (list->string (reverse acc))
              (loop (cons ch acc)))))))

(define (wait-readable port seconds)
  "True when PORT has input within SECONDS. A signal interrupting the wait counts as
nothing arriving, so a resize does not end the program."
  (or (char-ready? port)
      (let ((secs (inexact->exact (floor seconds))))
        (catch 'system-error
          (lambda ()
            (pair? (car (select (list port) '() '()
                                secs
                                (inexact->exact
                                 (round (* 1000000 (- seconds secs))))))))
          (lambda _ #f)))))

;; How long to wait for the rest of an escape sequence before taking a lone ESC to
;; mean the Escape key. Long enough for a slow link, short enough not to be felt.
(define %escape-timeout 0.03)

(define* (terminal-read-keys t #:optional (timeout 0))
  "Keys, and mouse records if mouse reports are on, that arrive within TIMEOUT
seconds, or the empty list. A sequence split across reads is held until the rest
arrives."
  (let ((port (terminal-input t)))
    (if (not (wait-readable port timeout))
        (flush-pending t)
        (let ((text (string-append (terminal-pending t) (read-available port))))
          (call-with-values (lambda () (parse-keys text))
            (lambda (keys rest)
              (set-terminal-pending! t rest)
              (if (string-null? rest)
                  keys
                  (append keys
                          (if (wait-readable port %escape-timeout)
                              (terminal-read-keys t 0)
                              (flush-pending t))))))))))

(define (flush-pending t)
  (let ((pending (terminal-pending t)))
    (set-terminal-pending! t "")
    (if (string-null? pending)
        '()
        (call-with-values (lambda () (parse-keys pending #:final? #t))
          (lambda (keys rest) keys)))))
