;;; What every widget draws with: the left bar, background fills, and the clock.

(define-module (promptui widgets common)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:export (%left-bar
            %border-width
            now-ms
            fill-background!
            draw-left-bar!
            draw-text!))

;; The heavy vertical bar OpenCode marks a block with.
(define %left-bar "┃")         ; ┃
(define %border-width 1)

(define (now-ms)
  "A monotonic clock in milliseconds. Widgets that animate take the current time as
an argument defaulting to this, which is what lets a test step them exactly."
  (quotient (* 1000 (get-internal-real-time)) internal-time-units-per-second))

(define (fill-background! buf area style)
  (buffer-fill! buf area " " style))

(define* (draw-left-bar! buf area style #:optional (top (rect-y area))
                         (bottom (rect-bottom area)))
  "The bar down the left edge of AREA, from row TOP to before row BOTTOM."
  (do ((y top (+ y 1))) ((>= y bottom))
    (buffer-set-cell! buf (rect-x area) y %left-bar style)))

(define (draw-text! buf x y x-max text style)
  "TEXT from X,Y clipped before X-MAX. Returns the column after the last cell."
  (buffer-set-string! buf x y text style x-max))
