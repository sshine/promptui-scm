;;; The event loop: draw, wait for something to happen, react, and draw again.
;;;
;;; Something happening is a key, a tick, or input on one of the ports the
;;; application asked to watch, such as a socket to a server. Waiting is a single
;;; select over all of them, so the loop sleeps until there is work instead of polling.
;;;
;;; The loop owns the terminal for its duration: raw mode, the alternate screen, and
;;; resizes. However it ends, by returning or by raising, the terminal is handed back
;;; the way it was found. That uses catch rather than dynamic-wind, because under a
;;; fibers scheduler dynamic-wind would run the exit thunk every time the loop
;;; suspended, not once at the end.

(define-module (promptui app)
  #:use-module (promptui terminal)
  #:use-module ((promptui input) #:select (mouse?))
  #:use-module (srfi srfi-1)
  #:export (run-app))

(define (ready-ports ports seconds)
  "Those of PORTS with input within SECONDS. Buffered input counts as ready without
waiting, since select only sees the file descriptor."
  (let ((buffered (filter char-ready? ports)))
    (if (pair? buffered)
        buffered
        (let ((secs (inexact->exact (floor seconds))))
          (catch 'system-error
            (lambda ()
              (car (select ports '() '() secs
                           (inexact->exact (round (* 1000000 (- seconds secs)))))))
            ;; A signal, most often SIGWINCH, interrupted the wait.
            (lambda _ '()))))))

(define* (run-app terminal #:key
                  (draw (lambda (frame) #f))
                  (on-key (lambda (key) #f))
                  (on-mouse (lambda (mouse) #f))
                  (on-tick (lambda () #f))
                  (tick 0.1)
                  (watch '())
                  (running? (const #t)))
  "Run until RUNNING? returns false.

DRAW is called with a frame on every pass. ON-KEY is called with each key, and
ON-MOUSE with each mouse report when the terminal has them on. ON-TICK is called on
every pass, before drawing, whether or not anything happened; TICK is the longest
the loop sleeps between passes, in seconds, and may be a procedure returning it, so
an idle application can sleep longer than an animating one. WATCH is a list of
(PORT . HANDLER), or a procedure returning one, consulted on every pass; HANDLER is
called with no arguments when PORT has input."
  (define resized? #f)
  (define old-handler
    (sigaction SIGWINCH (lambda (sig) (set! resized? #t))))
  (define (restore!)
    (terminal-leave! terminal)
    (raw-mode-off!)
    (sigaction SIGWINCH (car old-handler) (cdr old-handler)))
  (raw-mode-on!)
  (terminal-enter! terminal)
  (let ((result
         (catch #t
           (lambda ()
             (let loop ()
               (when (running?)
                 (when resized?
                   (set! resized? #f)
                   (terminal-refresh-size! terminal)
                   (terminal-invalidate! terminal))
                 (on-tick)
                 (terminal-draw! terminal draw)
                 (let* ((input (terminal-input terminal))
                        (watch (if (procedure? watch) (watch) watch))
                        (ready (ready-ports (cons input (map car watch))
                                            (if (procedure? tick) (tick) tick))))
                   (when (memq input ready)
                     (for-each (lambda (event)
                                 (if (mouse? event) (on-mouse event) (on-key event)))
                               (terminal-read-keys terminal 0)))
                   (for-each (lambda (w)
                               (when (memq (car w) ready) ((cdr w))))
                             watch))
                 (loop)))
             (cons 'ok #t))
           (lambda (key . args) (cons key args)))))
    (restore!)
    (unless (eq? (car result) 'ok)
      (apply throw (car result) (cdr result)))
    #t))
