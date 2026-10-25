;;; A one-row status bar: the agent's mode, what it is doing, and where it is.
;;;
;;; The left side reads mode, activity and model. The right side reads working
;;; directory, latency and context use, and when the row is too narrow it gives up
;;; segments from its left end, the directory first, so the numbers survive longest.

(define-module (promptui widgets status-bar)
  #:use-module (promptui width)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (promptui theme)
  #:use-module (promptui padding)
  #:use-module (promptui widgets common)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:export (%modes
            mode-label
            mode-color
            mode-next
            format-token-count
            make-status-bar
            status-bar?
            status-bar-mode
            status-bar-set-mode!
            status-bar-cycle-mode!
            status-bar-set-activity!
            status-bar-set-model!
            status-bar-set-cwd!
            status-bar-set-latency!
            status-bar-word-count
            status-bar-set-word-count!
            status-bar-usage-percent
            status-bar-set-padding!
            status-bar-height
            status-bar-render!))

;;; Modes, cycled by Tab: talk without acting, plan, act with permission, act
;;; within guardrails, and act without limits.

(define %modes '(talk plan accept auto ralph))

(define (mode-label mode)
  (case mode
    ((talk) "Talk") ((plan) "Plan") ((accept) "Accept")
    ((auto) "Auto") ((ralph) "Ralph")
    (else (symbol->string mode))))

(define (mode-color mode theme)
  (case mode
    ((talk) (theme-info theme))
    ((plan) (theme-primary theme))
    ((accept) (theme-secondary theme))
    ((auto) (theme-success theme))
    ((ralph) (theme-accent theme))
    (else (theme-text theme))))

(define (mode-next mode)
  (let ((tail (memq mode %modes)))
    (if (and tail (pair? (cdr tail))) (cadr tail) (car %modes))))

(define (format-token-count n)
  "1.2m, 3.4k, or the count itself below a thousand."
  (cond
   ((>= n 1000000) (format #f "~,1fm" (/ n 1000000.0)))
   ((>= n 1000) (format #f "~,1fk" (/ n 1000.0)))
   (else (number->string n))))

(define-record-type <status-bar>
  (%make-status-bar mode word-count context-window cwd padding activity latency
                    model theme)
  status-bar?
  (mode           status-bar-mode status-bar-set-mode!)
  (word-count     status-bar-word-count status-bar-set-word-count!)
  (context-window sb-context-window)
  (cwd            sb-cwd status-bar-set-cwd!)
  (padding        sb-padding status-bar-set-padding!)
  ;; Shown after the mode, e.g. a spinner frame and a verb. Empty to hide.
  (activity       sb-activity status-bar-set-activity!)
  ;; Milliseconds, or #f when unknown.
  (latency        sb-latency status-bar-set-latency!)
  (model          sb-model status-bar-set-model!)
  (theme          sb-theme))

(define* (make-status-bar theme #:key (context-window 200000) (model "")
                          (padding (padding-uniform 0)))
  (%make-status-bar 'talk 0 context-window "" padding "" #f model theme))

(define (status-bar-cycle-mode! sb)
  "Move to the next mode. Returns the new mode's colour."
  (status-bar-set-mode! sb (mode-next (status-bar-mode sb)))
  (mode-color (status-bar-mode sb) (sb-theme sb)))

(define (status-bar-usage-percent sb)
  (let ((window (sb-context-window sb)))
    (if (zero? window) 0.0 (* 100.0 (/ (status-bar-word-count sb) window)))))

(define (status-bar-height sb)
  (+ 1 (padding-vertical (sb-padding sb))))

(define %sep " · ")

(define (status-bar-render! sb area buf)
  (unless (rect-empty? area)
    (let* ((theme (sb-theme sb))
           (pad (sb-padding sb))
           (bg (theme-bg theme))
           (muted (make-style #:fg (theme-text-muted theme) #:bg bg))
           (y (+ (rect-y area) (padding-top pad)))
           (x-max (- (rect-right area) (padding-right pad))))
      (fill-background! buf area (make-style #:bg bg))
      (when (< y (rect-bottom area))
        (let* ((parts
                (append
                 (list (cons (mode-label (status-bar-mode sb))
                             (make-style #:fg (mode-color (status-bar-mode sb) theme)
                                         #:bg bg #:modifiers '(bold))))
                 (if (string-null? (sb-activity sb))
                     '()
                     (list (cons %sep muted)
                           (cons (sb-activity sb)
                                 (make-style #:fg (theme-text theme) #:bg bg))))
                 (if (string-null? (sb-model sb))
                     '()
                     (list (cons %sep muted)
                           (cons (sb-model sb)
                                 (make-style #:fg (theme-text theme) #:bg bg
                                             #:modifiers '(bold)))))))
               (x (fold (lambda (part x)
                          (draw-text! buf x y x-max (car part) (cdr part)))
                        (+ (rect-x area) (padding-left pad))
                        parts))
               (right (filter-map
                       identity
                       (list (and (not (string-null? (sb-cwd sb))) (sb-cwd sb))
                             (and (sb-latency sb) (format #f "~ams" (sb-latency sb)))
                             (format #f "~a tokens (~,1f%)"
                                     (format-token-count (status-bar-word-count sb))
                                     (status-bar-usage-percent sb))))))
          ;; Drop segments from the left until the rest fits beside the left side.
          (let loop ((segments right))
            (unless (null? segments)
              (let ((text (string-join segments %sep)))
                (if (> (- x-max x) (string-width text))
                    (draw-text! buf (- x-max (string-width text)) y x-max text muted)
                    (loop (cdr segments))))))))))
  buf)
