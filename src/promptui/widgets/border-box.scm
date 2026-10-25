;;; Chrome around content that the box does not own.
;;;
;;; A border box draws a left bar, a square box or a rounded box, with an optional
;;; title in the top edge, and reports the inner area that is left. The caller draws
;;; its content there, so any widget can be bordered without knowing about borders.

(define-module (promptui widgets border-box)
  #:use-module (promptui width)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (promptui theme)
  #:use-module (promptui padding)
  #:use-module (promptui widgets common)
  #:use-module (srfi srfi-9)
  #:export (make-border-box
            border-box?
            border-box-kind
            border-box-set-title!
            border-box-set-border-color!
            border-box-set-padding!
            border-box-h-overhead
            border-box-v-overhead
            border-box-inner
            border-box-render!))

(define %square '("┌" "┐" "└" "┘"))    ; ┌ ┐ └ ┘
(define %rounded '("╭" "╮" "╰" "╯"))   ; ╭ ╮ ╰ ╯
(define %horizontal "─")                              ; ─
(define %vertical "│")                                ; │

(define-record-type <border-box>
  (%make-border-box theme padding kind color title)
  border-box?
  (theme   bb-theme)
  (padding bb-padding border-box-set-padding!)
  ;; left-bar, square or rounded.
  (kind    border-box-kind)
  (color   bb-color border-box-set-border-color!)
  (title   bb-title border-box-set-title!))

(define* (make-border-box theme kind #:key (color (theme-border theme)) (title #f)
                          (padding (padding-uniform 0)))
  (%make-border-box theme padding kind color title))

(define (edges bb)
  "Border cells on the left, right, top and bottom."
  (if (eq? (border-box-kind bb) 'left-bar) '(1 0 0 0) '(1 1 1 1)))

(define (border-box-h-overhead bb)
  (let ((e (edges bb)))
    (+ (car e) (cadr e) (padding-horizontal (bb-padding bb)))))

(define (border-box-v-overhead bb)
  (let ((e (edges bb)))
    (+ (caddr e) (cadddr e) (padding-vertical (bb-padding bb)))))

(define (border-box-inner bb outer)
  "The area left for content inside OUTER."
  (let ((e (edges bb))
        (pad (bb-padding bb)))
    (rect-inset outer
                #:left (+ (car e) (padding-left pad))
                #:right (+ (cadr e) (padding-right pad))
                #:top (+ (caddr e) (padding-top pad))
                #:bottom (+ (cadddr e) (padding-bottom pad)))))

(define (border-box-render! bb area buf)
  "Draw the chrome. Draw the content into border-box-inner afterwards."
  (unless (rect-empty? area)
    (let* ((theme (bb-theme bb))
           (bg-style (make-style #:bg (theme-panel-bg theme)))
           (border-style (theme-fg-style theme (bb-color bb))))
      (fill-background! buf area bg-style)
      (case (border-box-kind bb)
        ((left-bar) (draw-left-bar! buf area border-style))
        (else
         (when (and (>= (rect-width area) 2) (>= (rect-height area) 2))
           (let ((corners (if (eq? (border-box-kind bb) 'rounded) %rounded %square))
                 (left (rect-x area))
                 (right (- (rect-right area) 1))
                 (top (rect-y area))
                 (bottom (- (rect-bottom area) 1)))
             (do ((x (+ left 1) (+ x 1))) ((>= x right))
               (buffer-set-cell! buf x top %horizontal border-style)
               (buffer-set-cell! buf x bottom %horizontal border-style))
             (do ((y (+ top 1) (+ y 1))) ((>= y bottom))
               (buffer-set-cell! buf left y %vertical border-style)
               (buffer-set-cell! buf right y %vertical border-style))
             (buffer-set-cell! buf left top (car corners) border-style)
             (buffer-set-cell! buf right top (cadr corners) border-style)
             (buffer-set-cell! buf left bottom (caddr corners) border-style)
             (buffer-set-cell! buf right bottom (cadddr corners) border-style)
             ;; "─ Title " after the corner; the rest of the edge stays a line.
             (let ((title (bb-title bb)))
               (when (and title (> right (+ left 2)))
                 (let* ((title-style (make-style #:fg (theme-text theme)
                                                 #:bg (theme-panel-bg theme)))
                        (shown (string-take-width title (max 0 (- (rect-width area) 4))))
                        (x (draw-text! buf (+ left 2) top right " " title-style))
                        (x (draw-text! buf x top right shown title-style)))
                   (draw-text! buf x top right " " title-style))))))))))
  buf)
