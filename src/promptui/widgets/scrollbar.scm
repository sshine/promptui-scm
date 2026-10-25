;;; A vertical scrollbar with half-cell precision.
;;;
;;; The track is treated as twice as tall as it is, and the thumb is drawn with
;;; upper and lower half blocks where it starts or ends in the middle of a cell, so
;;; it moves smoothly rather than in whole-row jumps. It draws nothing when the
;;; content fits.

(define-module (promptui widgets scrollbar)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (srfi srfi-9)
  #:export (make-scrollbar
            scrollbar?
            scrollbar-content-length
            scrollbar-viewport-length
            scrollbar-position
            scrollbar-visible?
            scrollbar-render!))

(define %full-block "█")      ; █
(define %top-half "▀")        ; ▀
(define %bottom-half "▄")     ; ▄

(define-record-type <scrollbar>
  (%make-scrollbar content-length viewport-length position track-bg thumb-fg)
  scrollbar?
  (content-length  scrollbar-content-length)
  (viewport-length scrollbar-viewport-length)
  (position        scrollbar-position)
  (track-bg        scrollbar-track-bg)
  (thumb-fg        scrollbar-thumb-fg))

(define* (make-scrollbar content-length viewport-length position
                         #:key (track-bg (rgb 37 37 39)) (thumb-fg (rgb 154 158 163)))
  "A scrollbar over CONTENT-LENGTH rows of which VIEWPORT-LENGTH are visible,
starting at row POSITION."
  (%make-scrollbar content-length viewport-length position track-bg thumb-fg))

;; Rounding half away from zero, as Rust's f64::round does. Scheme's round goes to
;; even, which places the thumb differently at exact midpoints.
(define (round-half-up x)
  (inexact->exact (floor (+ x 1/2))))

(define (scrollbar-visible? sb)
  (and (> (scrollbar-content-length sb) (scrollbar-viewport-length sb))
       (> (scrollbar-viewport-length sb) 0)))

(define (scrollbar-render! sb area buf)
  (when (and (scrollbar-visible? sb) (not (rect-empty? area)))
    (let* ((track-height (rect-height area))
           (track-style (make-style #:bg (scrollbar-track-bg sb)))
           (thumb-style (make-style #:fg (scrollbar-thumb-fg sb)
                                    #:bg (scrollbar-track-bg sb)))
           (content (scrollbar-content-length sb))
           (viewport (scrollbar-viewport-length sb))
           (virtual-track (* 2 track-height))
           ;; At least one whole cell, so the thumb never vanishes.
           (virtual-thumb (max 2 (round-half-up (* virtual-track (/ viewport content)))))
           (scroll-range (max 0 (- content viewport)))
           (ratio (if (> scroll-range 0) (/ (scrollbar-position sb) scroll-range) 0))
           (virtual-pos (round-half-up
                         (* ratio (max 0 (- virtual-track virtual-thumb))))))
      (do ((row 0 (+ row 1))) ((= row track-height))
        (let* ((y (+ (rect-y area) row))
               (top (* 2 row))
               (bottom (+ top 2))
               (overlap-top (max top virtual-pos))
               (overlap-bottom (min bottom (+ virtual-pos virtual-thumb))))
          (if (< overlap-top overlap-bottom)
              (buffer-set-cell! buf (rect-x area) y
                                (cond ((>= (- overlap-bottom overlap-top) 2) %full-block)
                                      ((= overlap-top top) %top-half)
                                      (else %bottom-half))
                                thumb-style)
              (buffer-set-cell! buf (rect-x area) y " " track-style))))))
  buf)
