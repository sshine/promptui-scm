;;; Rectangles and the layout that divides one into several.
;;;
;;; A rect is a position and a size in cells, measured from the top-left of the
;;; screen. Arithmetic on it saturates at zero, as in ratatui, so a widget asked to
;;; draw into a space smaller than its chrome gets an empty area rather than a
;;; negative one.

(define-module (promptui rect)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-rect
            rect?
            rect-x
            rect-y
            rect-width
            rect-height
            rect-right
            rect-bottom
            rect-empty?
            rect-contains?
            rect-intersection
            rect-inset
            rect-row
            sub0
            layout-vertical
            layout-horizontal))

(define-immutable-record-type <rect>
  (%make-rect x y width height)
  rect?
  (x      rect-x)
  (y      rect-y)
  (width  rect-width)
  (height rect-height))

(define (sub0 a . bs)
  "A minus every B, but never below zero."
  (max 0 (apply - a bs)))

(define (make-rect x y width height)
  (%make-rect x y (max 0 width) (max 0 height)))

(set-record-type-printer! <rect>
  (lambda (r port)
    (format port "#<rect ~a,~a ~ax~a>"
            (rect-x r) (rect-y r) (rect-width r) (rect-height r))))

(define (rect-right r) (+ (rect-x r) (rect-width r)))
(define (rect-bottom r) (+ (rect-y r) (rect-height r)))

(define (rect-empty? r)
  (or (zero? (rect-width r)) (zero? (rect-height r))))

(define (rect-contains? r x y)
  (and (>= x (rect-x r)) (< x (rect-right r))
       (>= y (rect-y r)) (< y (rect-bottom r))))

(define (rect-intersection a b)
  (let ((x (max (rect-x a) (rect-x b)))
        (y (max (rect-y a) (rect-y b))))
    (make-rect x y
               (- (min (rect-right a) (rect-right b)) x)
               (- (min (rect-bottom a) (rect-bottom b)) y))))

(define* (rect-inset r #:key (left 0) (right 0) (top 0) (bottom 0))
  "R with the given number of cells taken off each side."
  (make-rect (+ (rect-x r) (min left (rect-width r)))
             (+ (rect-y r) (min top (rect-height r)))
             (sub0 (rect-width r) left right)
             (sub0 (rect-height r) top bottom)))

(define (rect-row r n)
  "The Nth row of R as a rect one cell high."
  (make-rect (rect-x r) (+ (rect-y r) n) (rect-width r) 1))

;;; Layout
;;;
;;; A constraint is one of
;;;
;;;   (length N)      exactly N cells
;;;   (percentage P)  P percent of the space
;;;   (min N)         at least N cells, and a share of what is left
;;;   (max N)         a share of what is left, but no more than N cells
;;;   (fill W)        a share of what is left, weighted by W
;;;
;;; Fixed constraints are satisfied first, in order, and clipped to the space that
;;; remains, so a layout too large for its area loses its last parts first. What is
;;; left over is shared by the flexible ones.

(define (fixed-size constraint total)
  (case (car constraint)
    ((length) (cadr constraint))
    ((percentage) (quotient (* total (cadr constraint)) 100))
    ((min) (cadr constraint))
    (else 0)))

(define (flex-weight constraint)
  (case (car constraint)
    ((min max) 1)
    ((fill) (if (pair? (cdr constraint)) (cadr constraint) 1))
    (else 0)))

(define (solve constraints total)
  (let* ((fixed (let loop ((cs constraints) (left total) (acc '()))
                  (if (null? cs)
                      (reverse acc)
                      (let ((n (min left (fixed-size (car cs) total))))
                        (loop (cdr cs) (- left n) (cons n acc))))))
         (spare (sub0 total (apply + fixed))))
    ;; A max constraint that is satisfied below its share hands the rest back, so
    ;; shares are computed again until no max is binding.
    (let loop ((capped '()))
      (let* ((weights (map (lambda (c i)
                             (if (memv i capped) 0 (flex-weight c)))
                           constraints (iota (length constraints))))
             (capped-total (apply + (map (lambda (i)
                                           (cadr (list-ref constraints i)))
                                         capped)))
             (pool (sub0 spare capped-total))
             (weight-sum (apply + weights))
             (shares (if (zero? weight-sum)
                         (map (const 0) weights)
                         (distribute pool weights weight-sum)))
             (newly (filter (lambda (i)
                              (let ((c (list-ref constraints i)))
                                (and (eq? (car c) 'max)
                                     (not (memv i capped))
                                     (> (list-ref shares i) (cadr c)))))
                            (iota (length constraints)))))
        (if (null? newly)
            (map (lambda (c n s i)
                   (+ n (if (memv i capped) (cadr c) s)))
                 constraints fixed shares (iota (length constraints)))
            (loop (append capped newly)))))))

(define (distribute pool weights weight-sum)
  "POOL split in proportion to WEIGHTS, with the rounding remainder going to the
earliest parts so that the shares always add up to POOL."
  (let* ((base (map (lambda (w) (quotient (* pool w) weight-sum)) weights))
         (rest (- pool (apply + base))))
    (let loop ((bs base) (ws weights) (rest rest) (acc '()))
      (cond
       ((null? bs) (reverse acc))
       ((and (> rest 0) (> (car ws) 0))
        (loop (cdr bs) (cdr ws) (- rest 1) (cons (+ 1 (car bs)) acc)))
       (else (loop (cdr bs) (cdr ws) rest (cons (car bs) acc)))))))

(define (layout-vertical area constraints)
  "AREA split into rows, top to bottom, one rect per constraint."
  (let loop ((sizes (solve constraints (rect-height area)))
             (y (rect-y area))
             (acc '()))
    (if (null? sizes)
        (reverse acc)
        (loop (cdr sizes) (+ y (car sizes))
              (cons (make-rect (rect-x area) y (rect-width area) (car sizes)) acc)))))

(define (layout-horizontal area constraints)
  "AREA split into columns, left to right, one rect per constraint."
  (let loop ((sizes (solve constraints (rect-width area)))
             (x (rect-x area))
             (acc '()))
    (if (null? sizes)
        (reverse acc)
        (loop (cdr sizes) (+ x (car sizes))
              (cons (make-rect x (rect-y area) (car sizes) (rect-height area)) acc)))))
