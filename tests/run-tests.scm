;;; Runs every tests/*-test.scm in its own process, so one suite's global state cannot
;;; decide another's outcome.

(use-modules (ice-9 ftw)
             (srfi srfi-1))

;; (current-filename) is #f under `guile -s`, so take the path we were invoked with.
(define test-dir
  (canonicalize-path (dirname (or (current-filename) (car (command-line))))))

(define (suite-files)
  (sort (filter (lambda (f) (string-suffix? "-test.scm" f))
                (or (scandir test-dir) '()))
        string<?))

(define (src-dir)
  (string-append (dirname test-dir) "/src"))

(define (run-one file)
  (let ((status (status:exit-val
                 (system* "guile" "--no-auto-compile"
                          "-L" (src-dir) "-L" test-dir
                          "-s" (string-append test-dir "/" file)))))
    (format #t "~a ~a~%" (if (eqv? status 0) "  ok" "FAIL") file)
    (eqv? status 0)))

(let* ((files (suite-files))
       (results (map run-one files))
       (failed (count not results)))
  (newline)
  (format #t "~a suite(s), ~a failed~%" (length files) failed)
  (exit (if (zero? failed) 0 1)))
