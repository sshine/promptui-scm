(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (harness)
             (promptui group))

;;; Items are (CLASS LABEL PHASE), enough for the grammar to read.

(define* (item class label #:optional (phase 'past)) (list class label phase))
(define (shape grouped)
  "The grouping with each item reduced to its label."
  (map (lambda (g)
         (if (eq? (car g) 'group)
             (cons 'group (map cadr (cdr g)))
             (cadr (cdr g))))
       grouped))
(define (group-of items) (shape (group-items items #:class car #:phase caddr)))

(run-suite
 "group"
 (lambda ()

   (test-group "a run of thinking and tool calls folds into one group"
     (test-equal '((group think bash think read think bash bash bash))
       (group-of (map (lambda (l) (item (if (eq? l 'think) 'process 'process) l))
                      '(think bash think read think bash bash bash)))))

   (test-group "prose inside a run joins it; the answer after it stands alone"
     (test-equal '(user (group think note1 read think note2 bash) answer)
       (group-of (list (item 'solo 'user)
                       (item 'process 'think)
                       (item 'prose 'note1)
                       (item 'process 'read)
                       (item 'process 'think)
                       (item 'prose 'note2)
                       (item 'process 'bash)
                       (item 'prose 'answer)))))

   (test-group "prose with no work in its stretch is no group"
     (test-equal '(user answer)
       (group-of (list (item 'solo 'user) (item 'prose 'answer)))))

   (test-group "a solo item breaks a run"
     (test-equal '((group think) warning (group bash))
       (group-of (list (item 'process 'think)
                       (item 'solo 'warning)
                       (item 'process 'bash)))))

   (test-group "the first live item ends grouping, and everything after it shows"
     (test-equal '((group think read) bash answer)
       (group-of (list (item 'process 'think)
                       (item 'process 'read)
                       (item 'process 'bash 'live)
                       (item 'prose 'answer)))))

   (test-group "summaries add up"
     (let ((s (summary-concat (list (make-summary 'thought #:span '(0 . 1000))
                                    (make-summary 'read)
                                    (make-summary 'thought #:span '(1500 . 3500))
                                    (make-summary 'command #:failed? #t)))))
       (test-equal '((thought . 2) (read . 1) (command . 1)) (summary-counts s))
       ;; From the first start to the last end, the gap between them included.
       (test-equal 3500 (summary-duration s))
       (test-equal 1 (summary-failures s))
       (test-equal '("2 thoughts" "1 read" "1 command")
         (summary-count-strings s '((read "read" "reads"))))
       (test-equal "3s" (format-duration (summary-duration s)))
       (test-equal "1m 12s" (format-duration 72000))))

   (test-group "combining is associative, and empty is its identity"
     (let ((triples
            (list (list (make-summary 'a #:span '(5 . 9)) (make-summary 'b) (make-summary 'a #:failed? #t))
                  (list summary-empty (make-summary 'c #:span '(1 . 3)) (make-summary 'b #:span '(2 . 20)))
                  (list (make-summary 'x) (make-summary 'y) (make-summary 'z)))))
       (define (same? s t)
         (and (equal? (summary-counts s) (summary-counts t))
              (equal? (summary-span s) (summary-span t))
              (= (summary-failures s) (summary-failures t))))
       (for-each (lambda (abc)
                   (let ((a (first abc)) (b (second abc)) (c (third abc)))
                     (test-assert (same? (summary-combine (summary-combine a b) c)
                                         (summary-combine a (summary-combine b c))))
                     (test-assert (same? a (summary-combine summary-empty a)))
                     (test-assert (same? a (summary-combine a summary-empty)))))
                 triples)))))
