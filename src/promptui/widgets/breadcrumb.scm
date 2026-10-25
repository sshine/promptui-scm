;;; A navigation trail from the root to the focused scope: main › research › deep.
;;;
;;; The last segment is the active one and is drawn bold. A trail one segment long
;;; says nothing, so it takes no height at all.

(define-module (promptui widgets breadcrumb)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (promptui theme)
  #:use-module (promptui padding)
  #:use-module (promptui widgets common)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-breadcrumb-segment
            breadcrumb-segment?
            breadcrumb-segment-label
            breadcrumb-segment-color
            breadcrumb-segment-active?
            make-breadcrumb
            breadcrumb?
            breadcrumb-segments
            breadcrumb-set-segments!
            breadcrumb-push!
            breadcrumb-pop!
            breadcrumb-depth
            breadcrumb-set-padding!
            breadcrumb-height
            breadcrumb-render!))

(define %separator " › ")      ; " › "

(define-record-type <breadcrumb-segment>
  (%make-segment label color active)
  breadcrumb-segment?
  (label  breadcrumb-segment-label)
  (color  breadcrumb-segment-color)
  (active breadcrumb-segment-active?))

(define* (make-breadcrumb-segment label color #:key (active #f))
  (%make-segment label color active))

(define (with-active seg active)
  (%make-segment (breadcrumb-segment-label seg) (breadcrumb-segment-color seg) active))

(define-record-type <breadcrumb>
  (%make-breadcrumb theme padding segments)
  breadcrumb?
  (theme    bc-theme)
  (padding  bc-padding breadcrumb-set-padding!)
  (segments breadcrumb-segments breadcrumb-set-segments!))

(define* (make-breadcrumb theme #:key (padding (padding-uniform 0)))
  (%make-breadcrumb theme padding '()))

(define (mark-last-active segments)
  (let ((n (length segments)))
    (map (lambda (s i) (with-active s (= i (- n 1)))) segments (iota n))))

(define (breadcrumb-push! bc segment)
  "Add SEGMENT at the end, which makes it the active one."
  (breadcrumb-set-segments! bc (mark-last-active
                                (append (breadcrumb-segments bc) (list segment)))))

(define (breadcrumb-pop! bc)
  "Remove and return the last segment, or #f when there is none."
  (let ((segs (breadcrumb-segments bc)))
    (and (pair? segs)
         (begin
           (breadcrumb-set-segments! bc (mark-last-active (drop-right segs 1)))
           (last segs)))))

(define (breadcrumb-depth bc) (length (breadcrumb-segments bc)))

(define (breadcrumb-height bc)
  (if (<= (breadcrumb-depth bc) 1)
      0
      (+ 1 (padding-vertical (bc-padding bc)))))

(define (breadcrumb-render! bc area buf)
  (unless (or (rect-empty? area) (<= (breadcrumb-depth bc) 1))
    (let* ((theme (bc-theme bc))
           (pad (bc-padding bc))
           (y (+ (rect-y area) (padding-top pad)))
           (x-max (- (rect-right area) (padding-right pad))))
      (fill-background! buf area (make-style #:bg (theme-panel-bg theme)))
      (when (< y (rect-bottom area))
        (fold (lambda (seg i x)
                (let ((x (if (> i 0)
                             (draw-text! buf x y x-max %separator (theme-muted-style theme))
                             x)))
                  (draw-text! buf x y x-max (breadcrumb-segment-label seg)
                              (make-style #:fg (breadcrumb-segment-color seg)
                                          #:bg (theme-panel-bg theme)
                                          #:modifiers (if (breadcrumb-segment-active? seg)
                                                          '(bold)
                                                          '())))))
              (+ (rect-x area) (padding-left pad))
              (breadcrumb-segments bc)
              (iota (breadcrumb-depth bc))))))
  buf)
