;;; A chat interface with a scripted assistant, ported from promptui-rs's chat example.
;;;
;;;   guile -L src examples/chat.scm
;;;
;;; Type and press Enter to get a mock reply streamed back. A line starting with $
;;; runs as a shell command. Tab cycles the mode; in Accept mode a message becomes a
;;; plan whose steps tick off one by one. Shift+Enter or Alt+Enter inserts a newline,
;;; PageUp, PageDown and the mouse wheel scroll, the scrollbar can be dragged, and
;;; Escape or Ctrl+C quits.

(use-modules (promptui style)
             (promptui rect)
             (promptui text)
             (promptui input)
             (promptui theme)
             (promptui padding)
             (promptui terminal)
             (promptui app)
             (promptui widgets common)
             (promptui widgets text-field)
             (promptui widgets message-list)
             (promptui widgets scroll-box)
             (promptui widgets status-bar)
             (promptui widgets spinner)
             (promptui widgets notice)
             (promptui widgets streaming-text)
             (srfi srfi-1)
             (ice-9 popen)
             (ice-9 textual-ports))

(define theme %opencode-theme)

(define field (make-text-field theme #:padding (padding-uniform 1)))
(define messages (make-message-list theme #:padding (padding-uniform 1)))
(define scroll (make-scroll-box))
(define status (make-status-bar theme #:model "mock" #:padding (padding-uniform 1)))
(define spinner (make-spinner %wave-ripple))
(define notice (make-notice theme #:padding (padding-uniform 1)))

(define streaming #f)
(define verb "")
(define running #t)
;; (DUE-MS . THUNK), run once their time has come.
(define timers '())
;; Shell commands in flight, as (PORT . HANDLER) for the loop to watch.
(define shells '())

(define (after! ms thunk)
  (set! timers (cons (cons (+ (now-ms) ms) thunk) timers)))

(define (run-timers!)
  (let* ((now (now-ms))
         (due (filter (lambda (t) (<= (car t) now)) timers)))
    (set! timers (remove (lambda (t) (<= (car t) now)) timers))
    (for-each (lambda (t) ((cdr t))) due)))

(define %verbs '("Pondering" "Musing" "Considering" "Ruminating" "Deliberating"))
(define %replies
  '("That is a good question. The short answer is that it depends on what you are optimizing for: latency, throughput or the time it takes to understand the code later."
    "I would start by writing the test that fails, then make it pass with the smallest change that could possibly work."
    "Here is a thought: the widget does not need to own its scroll position. Let the container do that and the widget stays simple."))

(define (pick xs) (list-ref xs (random (length xs))))

(define (busy! what)
  (set! verb what)
  (spinner-start! spinner))

(define (idle!)
  (spinner-stop! spinner)
  (status-bar-set-activity! status ""))

(define (stream-reply! text)
  (message-list-push-message! messages 'assistant "")
  (let ((st (make-streaming-text theme)))
    (streaming-text-push! st text)
    (streaming-text-finish! st)
    (set! streaming st))
  (scroll-box-scroll-to-bottom! scroll))

(define (run-shell! command)
  (message-list-push-message! messages 'user
                              (line (styled "$ " (make-style #:fg (theme-warning theme)))
                                    command))
  (scroll-box-scroll-to-bottom! scroll)
  (busy! "Running")
  (let ((port (open-input-pipe (string-append command " 2>&1"))))
    (set! shells
          (cons (cons port
                      (lambda ()
                        (let ((out (get-string-all port)))
                          (close-pipe port)
                          (set! shells (remove (lambda (s) (eq? (car s) port)) shells))
                          (idle!)
                          (message-list-push-message!
                           messages 'system
                           (let ((trimmed (string-trim-right out)))
                             (if (string-null? trimmed) "(no output)" trimmed)))
                          (scroll-box-scroll-to-bottom! scroll))))
                shells))))

(define (plan! text)
  (message-list-push-message! messages 'user text)
  (let* ((steps (list "Read the relevant code" "Write a failing test"
                      "Make the change" "Run the suite"))
         (todo (message-list-push! messages
                                   (make-todo-list-block "Plan"
                                                         (map (lambda (s) (cons s #f)) steps)))))
    (busy! (pick %verbs))
    (let loop ((i 0) (delay 0))
      (when (< i (length steps))
        (let ((delay (+ delay 1500 (random 2000))))
          (after! delay
                  (lambda ()
                    (todo-list-block-check! todo i)
                    (stream-reply! (format #f "Done: ~a." (list-ref steps i)))
                    (if (every cdr (todo-list-block-items todo))
                        (begin (idle!) (notice-show! notice 'info "All tasks completed!"))
                        (set! verb (pick %verbs)))))
          (loop (+ i 1) delay))))
    (scroll-box-scroll-to-bottom! scroll)))

(define (submit! text)
  (cond
   ((string-prefix? "$" text) (run-shell! (string-trim-both (substring text 1))))
   ((eq? (status-bar-mode status) 'accept) (plan! text))
   (else
    (message-list-push-message! messages 'user text)
    (scroll-box-scroll-to-bottom! scroll)
    (status-bar-set-latency! status (+ 10 (random 140)))
    (busy! (pick %verbs))
    (after! (+ 1000 (random 2000))
            (lambda () (idle!) (stream-reply! (pick %replies)))))))

(define (on-key key)
  (let ((input (key->input key)))
    (cond
     ((eq? input 'esc) (set! running #f))
     ((eq? input 'tab)
      (text-field-set-border-color! field (status-bar-cycle-mode! status)))
     ((eq? input 'pageup) (scroll-box-page-up! scroll))
     ((eq? input 'pagedown) (scroll-box-page-down! scroll))
     ((eq? input 'paste) (text-field-input! field (key-text key)))
     (else
      (let ((result (text-field-input! field input)))
        (when (pair? result) (submit! (cdr result))))))))

(define (on-tick)
  (run-timers!)
  (spinner-tick! spinner)
  (notice-tick! notice)
  (when streaming
    (when (streaming-text-tick! streaming)
      (message-list-set-last-content! messages (streaming-text-revealed streaming))
      (scroll-box-scroll-to-bottom! scroll))
    (when (streaming-text-finished? streaming)
      (message-list-set-last-content! messages (streaming-text-revealed streaming))
      (set! streaming #f)))
  (when (spinner-active? spinner)
    (status-bar-set-activity! status (string-append (spinner-frame spinner) " " verb "...")))
  (status-bar-set-word-count! status (message-list-total-word-count messages)))

(define (draw frame)
  (let* ((area (frame-area frame))
         (buf (frame-buffer frame))
         (parts (layout-vertical area
                                 `((min 3)
                                   (length ,(status-bar-height status))
                                   (length ,(text-field-required-height
                                             field (rect-width area)))))))
    (scroll-box-render! scroll (car parts) buf (message-list-scrollable messages))
    (notice-render! notice (car parts) buf)
    (status-bar-render! status (cadr parts) buf)
    (text-field-render! field (caddr parts) buf)
    (let ((cursor (text-field-cursor-position field (caddr parts))))
      (when cursor (frame-set-cursor! frame (car cursor) (cdr cursor))))))

(text-field-set-border-color! field (mode-color (status-bar-mode status) theme))
(text-field-set-style-proc! field
                            (lambda (row col ch)
                              (and (= row 0) (= col 0) (char=? ch #\$)
                                   (make-style #:fg (theme-warning theme)))))
(status-bar-set-cwd! status
                     (let ((cwd (getcwd)) (home (getenv "HOME")))
                       (if (and home (string-prefix? home cwd))
                           (string-append "~" (substring cwd (string-length home)))
                           cwd)))
(set! *random-state* (random-state-from-platform))

(run-app (make-terminal #:mouse? #t)
         #:draw draw
         #:on-key on-key
         #:on-mouse (lambda (m) (scroll-box-handle-mouse! scroll m))
         #:on-tick on-tick
         #:tick (lambda () (if (or streaming (spinner-active? spinner)) 0.03 0.5))
         #:watch (lambda () shells)
         #:running? (lambda () running))
