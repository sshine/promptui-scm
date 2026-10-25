;;; A header line naming one agent in a tree of agents.
;;;
;;;   ━━ ● main (2m · 14k) ━━━━━━━━━━━━━━━━
;;;   ├─ ✓ research (12s · 2.1k)
;;;   │ └─ ● deep-analysis (3s)
;;;
;;; The root is set off by a heavy rule across the row; a child is indented under its
;;; ancestors and joined to them with tree lines.

(define-module (promptui widgets agent-scope)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (promptui theme)
  #:use-module (promptui padding)
  #:use-module (promptui widgets common)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:export (agent-status-icon
            agent-status-color
            format-duration
            format-tokens
            make-agent-scope-header
            agent-scope-header?
            agent-scope-header-status
            agent-scope-header-set-status!
            agent-scope-header-set-duration!
            agent-scope-header-set-token-count!
            agent-scope-header-set-padding!
            agent-scope-header-text
            agent-scope-header-height
            agent-scope-header-render!))

(define %tree-branch "├─")   ; ├─
(define %tree-last "└─")     ; └─
(define %tree-pipe "│")           ; │
(define %heavy-rule "━")          ; ━

;;; Status: active, done, error or cancelled.

(define (agent-status-icon status)
  (case status
    ((active) "●")       ; ●
    ((done) "✓")         ; ✓
    ((error) "✗")        ; ✗
    (else "—")))         ; —

(define (agent-status-color status theme)
  (case status
    ((active done) (theme-success theme))
    ((error) (theme-error theme))
    (else (theme-text-muted theme))))

(define (format-duration seconds)
  "12s, 2m, 1h."
  (let ((s (inexact->exact (floor seconds))))
    (cond
     ((< s 60) (format #f "~as" s))
     ((< s 3600) (format #f "~am" (quotient s 60)))
     (else (format #f "~ah" (quotient s 3600))))))

(define (format-tokens n)
  "940, 2.1k, 14k, 1.2M."
  (cond
   ((< n 1000) (number->string n))
   ((< n 10000) (format #f "~,1fk" (/ n 1000.0)))
   ((< n 1000000) (format #f "~ak" (inexact->exact (round (/ n 1000.0)))))
   (else (format #f "~,1fM" (/ n 1000000.0)))))

(define-record-type <agent-scope-header>
  (%make-header theme padding label color status duration tokens depth last?)
  agent-scope-header?
  (theme    ash-theme)
  (padding  ash-padding agent-scope-header-set-padding!)
  (label    ash-label)
  (color    ash-color)
  (status   agent-scope-header-status agent-scope-header-set-status!)
  ;; Seconds, or #f.
  (duration ash-duration agent-scope-header-set-duration!)
  (tokens   ash-tokens agent-scope-header-set-token-count!)
  ;; Zero for the root.
  (depth    ash-depth)
  ;; Whether this is the last sibling, which ends the branch with └─.
  (last?    ash-last?))

(define* (make-agent-scope-header theme label #:key
                                  (color (theme-secondary theme))
                                  (status 'active)
                                  (duration #f)
                                  (token-count 0)
                                  (depth 0)
                                  (last-sibling? #f)
                                  (padding (padding-uniform 0)))
  (%make-header theme padding label color status duration token-count depth last-sibling?))

(define (agent-scope-header-height h)
  (+ 1 (padding-vertical (ash-padding h))))

(define (stats h)
  (append (if (ash-duration h) (list (format-duration (ash-duration h))) '())
          (if (> (ash-tokens h) 0) (list (format-tokens (ash-tokens h))) '())))

(define (agent-scope-header-text h)
  "The header as plain text, e.g. \"● research (12s · 2.1k)\"."
  (let ((head (string-append (agent-status-icon (agent-scope-header-status h))
                             " " (ash-label h))))
    (if (null? (stats h))
        head
        (format #f "~a (~a)" head (string-join (stats h) " · ")))))

(define (agent-scope-header-render! h area buf)
  (unless (rect-empty? area)
    (let* ((theme (ash-theme h))
           (pad (ash-padding h))
           (bg-style (make-style #:bg (theme-panel-bg theme)))
           (line-style (theme-fg-style theme (theme-border theme)))
           (y (+ (rect-y area) (padding-top pad)))
           (x-max (- (rect-right area) (padding-right pad)))
           (text (lambda (x s style) (draw-text! buf x y x-max s style))))
      (fill-background! buf area bg-style)
      (when (< y (rect-bottom area))
        (let* ((x (+ (rect-x area) (padding-left pad)))
               (x (if (> (ash-depth h) 0)
                      (let ((x (let loop ((level 1) (x x))
                                 (if (and (< level (ash-depth h)) (<= (+ x 2) x-max))
                                     (loop (+ level 1) (text x (string-append %tree-pipe " ")
                                                             line-style))
                                     x))))
                        (text (text x (if (ash-last? h) %tree-last %tree-branch) line-style)
                              " " bg-style))
                      (text (text x (string-append %heavy-rule %heavy-rule) line-style)
                            " " bg-style)))
               (x (text x (agent-status-icon (agent-scope-header-status h))
                        (theme-fg-style theme (agent-status-color
                                               (agent-scope-header-status h) theme))))
               (x (text x " " bg-style))
               (x (text x (ash-label h) (theme-fg-style theme (ash-color h))))
               (x (if (null? (stats h))
                      x
                      (text x (format #f " (~a)" (string-join (stats h) " · "))
                            (theme-muted-style theme))))
               (x (text x " " bg-style)))
          (when (zero? (ash-depth h))
            (do ((x x (+ x 1))) ((>= x x-max))
              (buffer-set-cell! buf x y %heavy-rule line-style)))))))
  buf)
