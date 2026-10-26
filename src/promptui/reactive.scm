;;; Cells: values that know when they changed, and renders that know what they read.
;;;
;;; A cell holds a value and a version that moves whenever the value does. Reading a
;;; cell inside `track` records the cell and the version seen, so a cached render can
;;; later be checked with deps-current? and redone only when something it read has
;;; moved on.
;;;
;;; Nothing is pushed. No cell keeps a list of who depends on it, and nothing runs when
;;; a cell changes; the next frame asks. That keeps every procedure replaceable while
;;; the program runs: no subscription holds on to an old closure, and a render that is
;;; redefined simply reads whatever it reads the next time it is called.

(define-module (promptui reactive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-cell
            cell?
            cell-ref
            cell-peek
            cell-set!
            cell-update!
            cell-version
            track
            note-deps!
            deps-current?
            make-clock
            clock?
            clock-tick!
            clock-seconds))

(define-record-type <cell>
  (%make-cell value version)
  cell?
  (value   cell-value set-cell-value!)
  (version cell-version set-cell-version!))

(define (make-cell value) (%make-cell value 0))

;; The tracking frames of the renders running right now, innermost first. A render
;; drawn inside another (a member inside a group) reports to both, so the outer one
;; is redone when the inner one would be.
(define %frames '())

(define (note! c)
  (for-each (lambda (frame)
              (unless (assq c (car frame))
                (set-car! frame (cons (cons c (cell-version c)) (car frame)))))
            %frames))

(define (cell-ref c)
  "The value of C, recorded as read by every render being tracked."
  (note! c)
  (cell-value c))

(define (cell-peek c)
  "The value of C, without recording it."
  (cell-value c))

(define (cell-set! c value)
  "Replace the value of C. Setting an equal value is not a change: nothing that read
C needs doing again."
  (unless (equal? value (cell-value c))
    (set-cell-value! c value)
    (set-cell-version! c (+ 1 (cell-version c))))
  value)

(define (cell-update! c proc)
  (cell-set! c (proc (cell-value c))))

(define (track thunk)
  "Call THUNK. Returns two values: its result, and the cells it read as a list of
(CELL . VERSION)."
  (let ((frame (list '())))
    (set! %frames (cons frame %frames))
    (let ((result (catch #t
                    thunk
                    (lambda args
                      (set! %frames (cdr %frames))
                      (apply throw args)))))
      (set! %frames (cdr %frames))
      (values result (car frame)))))

(define (note-deps! deps)
  "Record DEPS as read, for a render that reuses a cached result instead of reading
the cells again."
  (for-each (lambda (d)
              (for-each (lambda (frame)
                          (unless (assq (car d) (car frame))
                            (set-car! frame (cons d (car frame)))))
                        %frames))
            deps))

(define (deps-current? deps)
  "True when no cell in DEPS has changed since it was read."
  (every (lambda (d) (= (cdr d) (cell-version (car d)))) deps))

;;; A clock
;;;
;;; Time is the one input that changes without anyone setting it. A clock is a cell of
;;; whole seconds that the loop ticks, so a render showing elapsed time reads it and is
;;; redone once a second, and a render that does not is never redone for the time.

(define-record-type <clock>
  (%make-clock cell)
  clock?
  (cell clock-cell))

(define* (make-clock #:optional (now-ms 0))
  (%make-clock (make-cell (quotient now-ms 1000))))

(define (clock-tick! clock now-ms)
  (cell-set! (clock-cell clock) (quotient now-ms 1000)))

(define (clock-seconds clock)
  (cell-ref (clock-cell clock)))
