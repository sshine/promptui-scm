;;; A scrolling log of components that are drawn again only when they change.
;;;
;;; A component is a key and a procedure of a width returning the component's lines,
;;; already wrapped. The procedure runs under `track`, and its lines are kept with the
;;; width, the caller's epoch and the cells it read. On the next frame it runs again
;;; only if one of those moved, so a log of a thousand settled items costs a hash
;;; lookup each, and the item whose cell changed is the one that is redrawn.
;;;
;;; The epoch is the caller's "everything changed": a new theme, or a redefined
;;; procedure that no cell knows about.
;;;
;;; Keys are compared with eq?, so a component keyed by the object it shows keeps its
;;; cache entry for as long as that object lives in the log.
;;;
;;; Scrolling is the scroll box's, bottom-anchored. While the reader is scrolled up,
;;; the row at the top of the view is remembered as a component and a row within it,
;;; and the next frame moves the offset so that row stays where it was, whatever
;;; grew, shrank or arrived below it.

(define-module (promptui widgets log-view)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (promptui text)
  #:use-module (promptui reactive)
  #:use-module (promptui widgets scroll-box)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-log-view
            log-view?
            log-view-render!
            log-view-key-at
            log-view-lines
            log-view-renders))

(define-record-type <log-view>
  (%make-log-view cache gap layout anchor renders)
  log-view?
  ;; Key to #(width epoch deps lines).
  (cache   lv-cache set-lv-cache!)
  ;; Blank rows between components.
  (gap     lv-gap)
  ;; What the last frame put where: a list of (KEY TOP BOTTOM) in screen rows, and
  ;; the area, for mapping a click back to a component.
  (layout  lv-layout set-lv-layout!)
  ;; (KEY . ROW) of the top visible row while scrolled up, or #f.
  (anchor  lv-anchor set-lv-anchor!)
  ;; How many times a component procedure has run, for tests and for curiosity.
  (renders log-view-renders set-lv-renders!))

(define* (make-log-view #:key (gap 1))
  (%make-log-view (make-hash-table) gap '() #f 0))

(define (component-lines lv key render width epoch new-cache)
  "KEY's lines at WIDTH, from the cache when nothing it depends on has moved."
  (let ((hit (hashq-ref (lv-cache lv) key)))
    (if (and hit
             (eqv? (vector-ref hit 0) width)
             (equal? (vector-ref hit 1) epoch)
             (deps-current? (vector-ref hit 2)))
        (begin
          (hashq-set! new-cache key hit)
          ;; A component drawn inside another reports what it read to the outer one.
          (note-deps! (vector-ref hit 2))
          (vector-ref hit 3))
        (call-with-values
            (lambda ()
              (track (lambda ()
                       (set-lv-renders! lv (+ 1 (log-view-renders lv)))
                       (catch #t
                         (lambda () (->text (render width)))
                         (lambda (k . args)
                           (list (line (format #f "[~a raised: ~a]" key k))))))))
          (lambda (lines deps)
            (hashq-set! new-cache key (vector width epoch deps lines))
            lines)))))

(define* (log-view-lines lv items width #:key (epoch 0))
  "Each of ITEMS, a list of (KEY . RENDER), as (KEY . LINES) at WIDTH. Entries for
keys no longer in ITEMS are dropped, so the cache is never larger than the log."
  (let* ((new-cache (make-hash-table))
         (out (map (lambda (item)
                     (cons (car item)
                           (component-lines lv (car item) (cdr item) width epoch new-cache)))
                   items)))
    (set-lv-cache! lv new-cache)
    out))

(define (total-height measured gap)
  (if (null? measured)
      0
      (+ (apply + (map (lambda (m) (length (cdr m))) measured))
         (* gap (- (length measured) 1)))))

(define (tops measured gap)
  "The first row of each measured component, counted from the top of the content."
  (let loop ((ms measured) (row 0) (acc '()))
    (if (null? ms)
        (reverse acc)
        (loop (cdr ms) (+ row (length (cdar ms)) gap) (cons row acc)))))

(define* (log-view-render! lv sb area buf items #:key (epoch 0))
  "Draw ITEMS into AREA through the scroll box SB. One column is kept for the
scrollbar whether or not it shows, so the width every component is drawn at does not
flip between frames and empty the cache."
  (unless (rect-empty? area)
    (let* ((width (max 1 (- (rect-width area) 1)))
           (measured (log-view-lines lv items width #:epoch epoch))
           (gap (lv-gap lv))
           (starts (tops measured gap))
           (total (total-height measured gap))
           (visible (rect-height area)))
      ;; Hold the remembered row in place before the scroll box clamps and draws.
      (let ((anchor (lv-anchor lv)))
        (when (and anchor (not (scroll-box-at-bottom? sb)))
          (let ((i (list-index (lambda (m) (eq? (car m) (car anchor))) measured)))
            (when i
              (let* ((wanted-top (+ (list-ref starts i) (cdr anchor)))
                     (wanted (max 0 (- total visible wanted-top))))
                (scroll-box-set-offset! sb wanted))))))
      (scroll-box-render!
       sb area buf
       (make-scrollable
        (lambda (w) total)
        (lambda (content-area buf offset)
          (let* ((top (max 0 (- total visible offset)))
                 (bottom (+ top (rect-height content-area))))
            (set-lv-layout! lv (list content-area))
            (for-each
             (lambda (m start)
               (let ((end (+ start (length (cdr m)))))
                 (when (and (> end top) (< start bottom))
                   (set-lv-layout! lv (append (lv-layout lv)
                                              (list (list (car m)
                                                          (+ (rect-y content-area) (- start top))
                                                          (+ (rect-y content-area) (- end top))))))
                   (let loop ((ls (cdr m)) (row start))
                     (when (and (pair? ls) (< row bottom))
                       (when (>= row top)
                         (render-line! buf (rect-x content-area)
                                       (+ (rect-y content-area) (- row top))
                                       (car ls) #:x-max (rect-right content-area)))
                       (loop (cdr ls) (+ row 1)))))))
             measured starts)
            ;; Remember which component holds the top row, for the next frame.
            (set-lv-anchor! lv
                            (and (> offset 0)
                                 (let ((i (list-index (lambda (s m)
                                                        (< top (+ s (length (cdr m)) gap)))
                                                      starts measured)))
                                   (and i (cons (car (list-ref measured i))
                                                (- top (list-ref starts i)))))))))))))
  buf)

(define (log-view-key-at lv x y)
  "The key of the component drawn at X,Y in the last frame, or #f."
  (let ((layout (lv-layout lv)))
    (and (pair? layout)
         (let ((area (car layout)))
           (and (>= x (rect-x area)) (< x (rect-right area))
                (any (lambda (entry)
                       (and (>= y (cadr entry)) (< y (caddr entry)) (car entry)))
                     (cdr layout)))))))
