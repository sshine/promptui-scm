;;; The cell buffer: what a frame is drawn into before any of it reaches a terminal.
;;;
;;; Every cell holds a symbol and a style. A symbol is a string rather than a
;;; character so that a cell can hold a base character with its combining marks. A
;;; wide character occupies its own cell and the one to its right, which holds the
;;; empty string and is skipped when the buffer is written out.
;;;
;;; Setting a cell patches its style rather than replacing it, as ratatui does, so a
;;; widget can fill a background and then write foreground-only text over it.

(define-module (promptui buffer)
  #:use-module (promptui width)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-buffer
            buffer?
            buffer-area
            buffer-symbol
            buffer-style
            buffer-set-cell!
            buffer-set-symbol!
            buffer-set-style!
            buffer-fill!
            buffer-set-string!
            buffer-reset!
            buffer-row-string
            buffer->lines))

(define-record-type <buffer>
  (%make-buffer area symbols styles)
  buffer?
  (area    buffer-area)
  (symbols buffer-symbols)
  (styles  buffer-styles))

(define (make-buffer area)
  (let ((n (* (rect-width area) (rect-height area))))
    (%make-buffer area (make-vector n " ") (make-vector n %reset-style))))

(define (buffer-reset! buf)
  (vector-fill! (buffer-symbols buf) " ")
  (vector-fill! (buffer-styles buf) %reset-style)
  buf)

(define (index buf x y)
  "The vector index of cell X,Y, or #f when it lies outside the buffer."
  (let ((area (buffer-area buf)))
    (and (rect-contains? area x y)
         (+ (* (- y (rect-y area)) (rect-width area))
            (- x (rect-x area))))))

(define (buffer-symbol buf x y)
  (let ((i (index buf x y)))
    (and i (vector-ref (buffer-symbols buf) i))))

(define (buffer-style buf x y)
  (let ((i (index buf x y)))
    (and i (vector-ref (buffer-styles buf) i))))

(define (symbol-width sym)
  (if (string-null? sym) 0 (string-width sym)))

(define (put-symbol! buf i x y sym)
  ;; Writing over either half of a wide character breaks it, and the half left
  ;; behind would otherwise be drawn as garbage or skipped.
  (let ((symbols (buffer-symbols buf)))
    (when (string-null? (vector-ref symbols i))
      (let ((left (index buf (- x 1) y)))
        (when left (vector-set! symbols left " "))))
    (when (> (symbol-width (vector-ref symbols i)) 1)
      (let ((right (index buf (+ x 1) y)))
        (when right (vector-set! symbols right " "))))
    (vector-set! symbols i sym)))

(define (buffer-set-symbol! buf x y sym)
  (let ((i (index buf x y)))
    (when i (put-symbol! buf i x y sym))
    buf))

(define (buffer-set-style! buf x y style)
  (let ((i (index buf x y)))
    (when i
      (vector-set! (buffer-styles buf) i
                   (style-patch (vector-ref (buffer-styles buf) i) style)))
    buf))

(define (buffer-set-cell! buf x y sym style)
  "Put SYM at X,Y and patch STYLE onto the cell. Outside the buffer it does nothing."
  (let ((i (index buf x y)))
    (when i
      (put-symbol! buf i x y sym)
      (vector-set! (buffer-styles buf) i
                   (style-patch (vector-ref (buffer-styles buf) i) style)))
    buf))

(define* (buffer-fill! buf area #:optional (sym " ") (style %reset-style))
  "Set every cell of AREA, clipped to the buffer."
  (let ((r (rect-intersection area (buffer-area buf))))
    (do ((y (rect-y r) (+ y 1))) ((>= y (rect-bottom r)))
      (do ((x (rect-x r) (+ x 1))) ((>= x (rect-right r)))
        (buffer-set-cell! buf x y sym style))))
  buf)

;; A tab has no width of its own, so it becomes spaces out to the next stop counted
;; from where the string started. Four columns is narrow enough to keep tool output
;; on screen and wide enough to keep its alignment readable.
(define %tab-width 4)

(define* (buffer-set-string! buf x y s style #:optional x-max)
  "Write S from X,Y in STYLE, stopping before X-MAX (exclusive, defaulting to the
right edge of the buffer). Returns the column after the last cell written, so a run
of differently styled text can continue from it.

Control characters are dropped: one written into a cell would move the terminal's
cursor and corrupt every cell after it. A combining mark joins the cell before it."
  (let ((limit (min (or x-max (rect-right (buffer-area buf)))
                    (rect-right (buffer-area buf))))
        (len (string-length s)))
    (let loop ((i 0) (cx x))
      (if (or (= i len) (>= cx limit))
          cx
          (let* ((ch (string-ref s i))
                 (w (char-width ch)))
            (cond
             ((char=? ch #\tab)
              (let* ((stop (* %tab-width (+ 1 (quotient (- cx x) %tab-width))))
                     (end (min limit (+ x stop))))
                (do ((c cx (+ c 1))) ((>= c end))
                  (buffer-set-cell! buf c y " " style))
                (loop (+ i 1) end)))
             ((and (zero? w) (> (char->integer ch) 159) (> cx x))
              ;; A combining mark: append it to the symbol already placed.
              (let ((prev (buffer-symbol buf (- cx 1) y)))
                (when prev
                  (buffer-set-symbol! buf (- cx 1) y (string-append prev (string ch)))))
              (loop (+ i 1) cx))
             ((zero? w) (loop (+ i 1) cx))
             ((> (+ cx w) limit) cx)
             (else
              (buffer-set-cell! buf cx y (string ch) style)
              (when (= w 2)
                (buffer-set-cell! buf (+ cx 1) y "" style))
              (loop (+ i 1) (+ cx w)))))))))

;;; Reading back, for tests and for anything that wants the text of a frame.

(define (buffer-row-string buf y)
  "Row Y as plain text, with the continuation half of every wide character left out."
  (let ((area (buffer-area buf)))
    (string-concatenate
     (map (lambda (x) (buffer-symbol buf x y))
          (iota (rect-width area) (rect-x area))))))

(define (buffer->lines buf)
  (let ((area (buffer-area buf)))
    (map (lambda (y) (buffer-row-string buf y))
         (iota (rect-height area) (rect-y area)))))
