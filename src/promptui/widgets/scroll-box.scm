;;; A bottom-anchored scrolling viewport.
;;;
;;; Offset zero shows the newest content, at the bottom; scrolling up moves the offset
;;; away from it. That is the natural anchor for a conversation, where new content
;;; arrives at the end and the reader is usually watching it arrive.
;;;
;;; What scrolls is a scrollable: a pair of procedures, one measuring the content's
;;; height at a width and one drawing it into an area at an offset. Any widget can be
;;; wrapped as one with make-scrollable. A scrollbar takes a column only when the
;;; content overflows.
;;;
;;; The box remembers where it was last drawn, so a mouse report, which carries only
;;; screen coordinates, can be routed to it: the wheel scrolls it, and pressing or
;;; dragging on the scrollbar moves the view to that point of the track.

(define-module (promptui widgets scroll-box)
  #:use-module (promptui rect)
  #:use-module (promptui input)
  #:use-module (promptui widgets scrollbar)
  #:use-module (srfi srfi-9)
  #:export (make-scrollable
            scrollable?
            scrollable-height
            scrollable-render!
            make-scroll-box
            scroll-box?
            scroll-box-offset
            scroll-box-scroll-up!
            scroll-box-scroll-down!
            scroll-box-page-up!
            scroll-box-page-down!
            scroll-box-scroll-to-bottom!
            scroll-box-set-offset!
            scroll-box-at-bottom?
            scroll-box-handle-mouse!
            scroll-box-render!))

(define-record-type <scrollable>
  (make-scrollable height render)
  scrollable?
  ;; (lambda (width) rows)
  (height scrollable-height-proc)
  ;; (lambda (area buf offset) ...), offset counted up from the bottom
  (render scrollable-render-proc))

(define (scrollable-height s width) ((scrollable-height-proc s) width))
(define (scrollable-render! s area buf offset) ((scrollable-render-proc s) area buf offset))

(define-record-type <scroll-box>
  (%make-scroll-box offset content-height viewport-height area bar dragging)
  scroll-box?
  (offset          scroll-box-offset set-offset!)
  (content-height  last-content-height set-last-content-height!)
  (viewport-height last-viewport-height set-last-viewport-height!)
  ;; Where the last render put the whole box and its scrollbar, the latter #f when
  ;; the content fitted.
  (area            last-area set-last-area!)
  (bar             last-bar set-last-bar!)
  (dragging        dragging? set-dragging!))

(define (make-scroll-box) (%make-scroll-box 0 0 0 #f #f #f))

(define (clamp! sb)
  (set-offset! sb (min (scroll-box-offset sb)
                       (max 0 (- (last-content-height sb) (last-viewport-height sb))))))

(define (scroll-box-scroll-up! sb n)
  "Towards older content."
  (set-offset! sb (+ (scroll-box-offset sb) n))
  (clamp! sb))

(define (scroll-box-scroll-down! sb n)
  "Towards newer content."
  (set-offset! sb (max 0 (- (scroll-box-offset sb) n))))

(define (half-viewport sb)
  (max 1 (quotient (last-viewport-height sb) 2)))

(define (scroll-box-page-up! sb) (scroll-box-scroll-up! sb (half-viewport sb)))
(define (scroll-box-page-down! sb) (scroll-box-scroll-down! sb (half-viewport sb)))

(define (scroll-box-scroll-to-bottom! sb) (set-offset! sb 0))

(define (scroll-box-set-offset! sb n)
  "Put the view N rows up from the bottom. It is clamped when the box is next drawn,
against the content as it is then, not as it was last time."
  (set-offset! sb (max 0 n)))
(define (scroll-box-at-bottom? sb) (zero? (scroll-box-offset sb)))

(define (scroll-box-render! sb area buf content)
  "Draw CONTENT, a scrollable, into AREA at the current offset."
  (unless (rect-empty? area)
    (let* ((full-height (scrollable-height content (rect-width area)))
           (overflow? (> full-height (rect-height area)))
           (content-width (if overflow? (max 0 (- (rect-width area) 1)) (rect-width area)))
           (content-area (make-rect (rect-x area) (rect-y area)
                                    content-width (rect-height area)))
           ;; Measured again at the narrower width: losing a column to the scrollbar
           ;; can push wrapped lines down.
           (content-height (if overflow?
                               (scrollable-height content content-width)
                               full-height)))
      (set-last-content-height! sb content-height)
      (set-last-viewport-height! sb (rect-height area))
      (set-last-area! sb area)
      (set-last-bar! sb (and overflow?
                             (make-rect (+ (rect-x area) content-width) (rect-y area)
                                        1 (rect-height area))))
      (clamp! sb)
      (scrollable-render! content content-area buf (scroll-box-offset sb))
      (when overflow?
        (let* ((max-scroll (max 0 (- content-height (rect-height area))))
               (position (max 0 (- max-scroll (scroll-box-offset sb)))))
          (scrollbar-render! (make-scrollbar content-height (rect-height area) position)
                             (make-rect (+ (rect-x area) content-width) (rect-y area)
                                        1 (rect-height area))
                             buf)))))
  buf)

;; How many rows one notch of the wheel moves, as in most terminal programs.
(define %wheel-rows 3)

(define (inside? r x y)
  (and r (>= x (rect-x r)) (< x (rect-right r)) (>= y (rect-y r)) (< y (rect-bottom r))))

(define (jump-to-row! sb bar y)
  "Scroll so that the thumb is centred on row Y of BAR, as near as the ends allow."
  (let* ((track (rect-height bar))
         (content (last-content-height sb))
         (max-scroll (max 0 (- content (last-viewport-height sb))))
         (thumb (min track (max 1 (/ (* track (last-viewport-height sb)) (max 1 content)))))
         (room (- track thumb))
         (ratio (if (> room 0)
                    (min 1 (max 0 (/ (- (- y (rect-y bar)) (/ (- thumb 1) 2)) room)))
                    0))
         (position (inexact->exact (round (* ratio max-scroll)))))
    (set-offset! sb (- max-scroll position))))

(define (scroll-box-handle-mouse! sb m)
  "React to M, a mouse record, if it concerns the box where it was last drawn. True
when it did, so the caller can stop offering it elsewhere."
  (let ((x (mouse-x m))
        (y (mouse-y m))
        (bar (last-bar sb)))
    (case (mouse-kind m)
      ((scroll-up scroll-down)
       (and (inside? (last-area sb) x y)
            (begin
              (if (eq? (mouse-kind m) 'scroll-up)
                  (scroll-box-scroll-up! sb %wheel-rows)
                  (scroll-box-scroll-down! sb %wheel-rows))
              #t)))
      ((press)
       (and (eq? (mouse-button m) 'left) (inside? bar x y)
            (begin (set-dragging! sb #t) (jump-to-row! sb bar y) #t)))
      ;; A drag keeps hold of the thumb even when the pointer strays off the bar.
      ((drag)
       (and (dragging? sb) bar
            (begin (jump-to-row! sb bar y) #t)))
      ((release)
       (and (dragging? sb) (begin (set-dragging! sb #f) #t)))
      (else #f))))
