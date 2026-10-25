;;; Colours, modifiers and styles, and the SGR sequences that draw them.
;;;
;;; A colour is one of:
;;;
;;;   #f              unset: patching leaves whatever was there
;;;   default         the terminal's own foreground or background
;;;   a symbol        one of the sixteen ANSI names, e.g. red or bright-black
;;;   0 to 255        an index into the 256-colour palette
;;;   "#rrggbb"       a 24-bit colour
;;;
;;; A style is a foreground, a background and a list of modifiers. Styles are
;;; immutable, and combining them follows ratatui: patching lays one style over
;;; another, so a cell's background fill survives text written over it in a
;;; foreground-only style.

(define-module (promptui style)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (rgb
            color?
            %modifiers
            make-style
            style?
            style-fg
            style-bg
            style-modifiers
            %reset-style
            style-patch
            style-add-modifier
            style-remove-modifier
            style-has-modifier?
            style=?
            style->sgr))

(define %ansi-colors
  '((black . 0) (red . 1) (green . 2) (yellow . 3)
    (blue . 4) (magenta . 5) (cyan . 6) (white . 7)
    (gray . 7)
    (bright-black . 8) (dark-gray . 8) (bright-red . 9) (bright-green . 10)
    (bright-yellow . 11) (bright-blue . 12) (bright-magenta . 13)
    (bright-cyan . 14) (bright-white . 15)))

(define (rgb r g b)
  "A 24-bit colour from three components in 0-255."
  (string-append "#" (hex2 r) (hex2 g) (hex2 b)))

(define (hex2 n)
  (let ((s (number->string (max 0 (min 255 (inexact->exact (round n)))) 16)))
    (if (= 1 (string-length s)) (string-append "0" s) s)))

(define (rgb-string? c)
  (and (string? c)
       (= 7 (string-length c))
       (char=? #\# (string-ref c 0))
       (string->number (substring c 1) 16)
       #t))

(define (color? c)
  (or (not c)
      (eq? c 'default)
      (and (symbol? c) (assq c %ansi-colors) #t)
      (and (exact-integer? c) (<= 0 c 255))
      (rgb-string? c)))

;; Ordered as SGR numbers them, which is the order they are emitted in.
(define %modifiers
  '((bold . 1) (dim . 2) (italic . 3) (underline . 4)
    (blink . 5) (reverse . 7) (hidden . 8) (strike . 9)))

(define-immutable-record-type <style>
  (%make-style fg bg modifiers)
  style?
  (fg        style-fg        set-style-fg)
  (bg        style-bg        set-style-bg)
  (modifiers style-modifiers set-style-modifiers))

(define (normalize-modifiers mods)
  (filter (lambda (m) (memq m mods)) (map car %modifiers)))

(define* (make-style #:key (fg #f) (bg #f) (modifiers '()))
  (%make-style fg bg (normalize-modifiers modifiers)))

(define %reset-style (make-style))

(define (style-patch base over)
  "OVER laid on BASE: its colours win where they are set, and the modifiers add up."
  (%make-style (or (style-fg over) (style-fg base))
               (or (style-bg over) (style-bg base))
               (normalize-modifiers (append (style-modifiers base)
                                            (style-modifiers over)))))

(define (style-add-modifier s . mods)
  (set-style-modifiers s (normalize-modifiers (append (style-modifiers s) mods))))

(define (style-remove-modifier s . mods)
  (set-style-modifiers s (remove (lambda (m) (memq m mods)) (style-modifiers s))))

(define (style-has-modifier? s mod)
  (and (memq mod (style-modifiers s)) #t))

(define (style=? a b)
  (and (equal? (style-fg a) (style-fg b))
       (equal? (style-bg a) (style-bg b))
       (equal? (style-modifiers a) (style-modifiers b))))

;;; SGR

(define (color-codes c background?)
  (cond
   ((or (not c) (eq? c 'default)) (list (if background? 49 39)))
   ((symbol? c)
    (let ((n (assq-ref %ansi-colors c)))
      (cond
       ;; An unknown name draws as the default rather than raising: a theme is data,
       ;; and a typo in it should not take a frame down.
       ((not n) (list (if background? 49 39)))
       ((< n 8) (list (+ n (if background? 40 30))))
       (else (list (+ (- n 8) (if background? 100 90)))))))
   ((exact-integer? c) (list (if background? 48 38) 5 c))
   ((rgb-string? c)
    (let ((n (string->number (substring c 1) 16)))
      (list (if background? 48 38) 2
            (ash n -16) (logand (ash n -8) 255) (logand n 255))))
   (else (list (if background? 49 39)))))

(define (style->sgr s)
  "The escape sequence that puts the terminal into exactly style S, starting from a
reset so that nothing set by an earlier cell carries over."
  (string-append
   "\x1b["
   (string-join
    (map number->string
         (append (list 0)
                 (filter-map (lambda (m) (assq-ref %modifiers m))
                             (style-modifiers s))
                 (color-codes (style-fg s) #f)
                 (color-codes (style-bg s) #t)))
    ";")
   "m"))
