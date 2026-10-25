;;; Shared test-suite entry point and helpers.
;;;
;;; SRFI-64's stock runner reports failures but exits 0, so a failing suite would look
;;; green to `just test` and to CI. It also prints a line per assertion, which buries
;;; the few lines that matter. run-suite fixes both.

(define-module (harness)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:export (run-suite
            render-lines
            count-cells))

(define (quiet-runner)
  (let ((runner (test-runner-simple)))
    (test-runner-on-test-end! runner
      (lambda (r)
        (case (test-result-kind r)
          ((fail xpass)
           (format #t "FAIL ~a:~a~%  form:     ~s~%  expected: ~s~%  actual:   ~s~%"
                   (or (test-result-ref r 'source-file) "?")
                   (or (test-result-ref r 'source-line) "?")
                   (test-result-ref r 'source-form)
                   (test-result-ref r 'expected-value)
                   (test-result-ref r 'actual-value)))
          (else #f))))
    (test-runner-on-group-begin! runner (lambda (r name count) #f))
    (test-runner-on-group-end! runner (lambda (r) #f))
    (test-runner-on-final! runner (lambda (r) #f))
    runner))

(define (run-suite name thunk)
  (let ((runner (quiet-runner)))
    (test-with-runner runner
      (test-begin name)
      (thunk)
      (test-end name))
    (let ((passed (test-runner-pass-count runner))
          (failed (+ (test-runner-fail-count runner)
                     (test-runner-xpass-count runner))))
      (format #t "~a: ~a passed, ~a failed~%" name passed failed)
      (exit (if (zero? failed) 0 1)))))

(define (render-lines width height draw)
  "Call DRAW with an area and a fresh buffer of WIDTH by HEIGHT, and return the
buffer's rows as strings. The snapshot a test compares against."
  (let* ((area (make-rect 0 0 width height))
         (buf (make-buffer area)))
    (draw area buf)
    (buffer->lines buf)))

(define (count-cells buf pred)
  "How many cells satisfy PRED, called with the symbol and the style."
  (let ((area (buffer-area buf)))
    (apply + (map (lambda (y)
                    (count (lambda (x) (pred (buffer-symbol buf x y) (buffer-style buf x y)))
                           (iota (rect-width area) (rect-x area))))
                  (iota (rect-height area) (rect-y area))))))
