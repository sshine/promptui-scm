;;; Padding: the space a widget leaves between its edge and its content, in cells.

(define-module (promptui padding)
  #:use-module (promptui rect)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-padding
            padding?
            padding-left
            padding-right
            padding-top
            padding-bottom
            padding-uniform
            padding-symmetric
            padding-horizontal
            padding-vertical
            padding-inset))

(define-immutable-record-type <padding>
  (%make-padding left right top bottom)
  padding?
  (left   padding-left)
  (right  padding-right)
  (top    padding-top)
  (bottom padding-bottom))

(define* (make-padding #:key (left 0) (right 0) (top 0) (bottom 0))
  (%make-padding left right top bottom))

(set-record-type-printer! <padding>
  (lambda (p port)
    (format port "#<padding l~a r~a t~a b~a>"
            (padding-left p) (padding-right p) (padding-top p) (padding-bottom p))))

(define (padding-uniform n)
  "N cells on every side."
  (%make-padding n n n n))

(define (padding-symmetric horizontal vertical)
  (%make-padding horizontal horizontal vertical vertical))

(define (padding-horizontal p) (+ (padding-left p) (padding-right p)))
(define (padding-vertical p) (+ (padding-top p) (padding-bottom p)))

(define (padding-inset area p)
  "AREA with P taken off its sides."
  (rect-inset area
              #:left (padding-left p) #:right (padding-right p)
              #:top (padding-top p) #:bottom (padding-bottom p)))
