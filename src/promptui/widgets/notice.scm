;;; A short-lived one-line notification in the top-right corner.
;;;
;;; It shows a level tag and a message between bars in the level's colour, with a bar
;;; along the bottom that fills as the notice ages, and dismisses itself once its
;;; time is up. Showing another replaces it.

(define-module (promptui widgets notice)
  #:use-module (promptui width)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (promptui theme)
  #:use-module (promptui padding)
  #:use-module (promptui widgets common)
  #:use-module (srfi srfi-9)
  #:export (notice-level-color
            notice-level-label
            make-notice
            notice?
            notice-show!
            notice-tick!
            notice-dismiss!
            notice-visible?
            notice-progress
            notice-duration
            notice-set-duration!
            notice-set-padding!
            notice-render!))

;;; Levels: debug, info, warn, error.

(define (notice-level-color level theme)
  (case level
    ((debug) (theme-text-muted theme))
    ((info) (theme-success theme))
    ((warn) (theme-warning theme))
    (else (theme-error theme))))

(define (notice-level-label level)
  (case level
    ((debug) "DBG") ((info) "INF") ((warn) "WRN") (else "ERR")))

(define-record-type <notice>
  (%make-notice theme active duration padding)
  notice?
  (theme    notice-theme)
  ;; (LEVEL MESSAGE SHOWN-AT) or #f.
  (active   notice-active set-notice-active!)
  ;; Milliseconds.
  (duration notice-duration notice-set-duration!)
  (padding  notice-padding notice-set-padding!))

(define* (make-notice theme #:key (duration 5000) (padding (padding-uniform 0)))
  (%make-notice theme #f duration padding))

(define* (notice-show! n level message #:optional (now (now-ms)))
  (set-notice-active! n (list level message now)))

(define* (notice-tick! n #:optional (now (now-ms)))
  "Dismiss the notice once its time has passed. Call it every tick."
  (let ((active (notice-active n)))
    (when (and active (>= (- now (caddr active)) (notice-duration n)))
      (set-notice-active! n #f))))

(define (notice-dismiss! n) (set-notice-active! n #f))

(define (notice-visible? n) (and (notice-active n) #t))

(define* (notice-progress n #:optional (now (now-ms)))
  "From 0.0, just shown, to 1.0, about to go. 0.0 when nothing is shown."
  (let ((active (notice-active n)))
    (cond
     ((not active) 0.0)
     ((<= (notice-duration n) 0) 1.0)
     (else (min 1.0 (exact->inexact (/ (- now (caddr active)) (notice-duration n))))))))

(define* (notice-render! n area buf #:optional (now (now-ms)))
  (let ((active (notice-active n)))
    (when active
      (let* ((theme (notice-theme n))
             (pad (notice-padding n))
             (level (car active))
             (message (cadr active))
             (label (notice-level-label level))
             ;; bar, space, label, two spaces, message, space, bar
             (want (+ 1 1 (string-width label) 2 (string-width message) 1 1))
             (width (min want (max 0 (- (rect-width area) (padding-horizontal pad)))))
             (height (min 3 (max 0 (- (rect-height area) (padding-vertical pad))))))
        (when (and (>= width 4) (> height 0))
          (let* ((x (+ (rect-x area) (max 0 (- (rect-width area) (padding-right pad) width))))
                 (y (+ (rect-y area) (padding-top pad)))
                 (color (notice-level-color level theme))
                 (bg (theme-element-bg theme))
                 (border-style (make-style #:fg color #:bg bg))
                 (bordered-row!
                  (lambda (row)
                    (buffer-fill! buf (make-rect x row width 1) " " (make-style #:bg bg))
                    (buffer-set-cell! buf x row "▎" border-style)
                    (buffer-set-cell! buf (+ x width -1) row "▕" border-style))))
            (bordered-row! y)
            (when (>= height 2)
              (bordered-row! (+ y 1))
              (let* ((x-max (+ x width -1))
                     (cx (draw-text! buf (+ x 2) (+ y 1) x-max label
                                     (make-style #:fg color #:bg bg #:modifiers '(bold)))))
                (draw-text! buf (+ cx 2) (+ y 1) (- x-max 1) message
                            (make-style #:fg (theme-text theme) #:bg bg))))
            (when (>= height 3)
              (bordered-row! (+ y 2))
              (let* ((inner (max 0 (- width 2)))
                     (filled (min inner (inexact->exact
                                         (floor (* inner (notice-progress n now)))))))
                (do ((i 0 (+ i 1))) ((= i filled))
                  (buffer-set-cell! buf (+ x 1 i) (+ y 2) "▁" border-style)))))))))
  buf)
