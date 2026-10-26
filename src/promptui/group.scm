;;; Grouping a log: which items fold together, and what a folded run says.
;;;
;;; Two small pieces.
;;;
;;; A grammar decides the runs. Every item has a class and a phase, both told by the
;;; caller. Classes:
;;;
;;;   process  work on the way to an answer: thinking, a tool call
;;;   prose    text written along the way
;;;   solo     anything that stands on its own: a prompt, an error, a warning
;;;
;;; The phase is live or past. The log is cut at every solo item and at the first live
;;; item; everything from that one on is shown as it is. Each stretch of past process
;;; and prose items between cuts forms
;;;
;;;   group := (process | prose)* process
;;;
;;; so prose inside a run joins it, and prose after the last process item, the answer
;;; the work led to, is left standing. A stretch with no process item is no group.
;;;
;;; A monoid says what a group is. Each item has a summary: how many of what, when it
;;; started and ended, how many failed. Counts and failures add up; times combine as
;;; the span from the earliest start to the latest end, so a group reports the wall
;;; time it covered, waits between its items included. summary-combine is associative and summary-empty is its identity,
;;; so a group's summary is the fold of its items' summaries, a run can grow one item
;;; at a time, and groups of groups would summarize the same way.

(define-module (promptui group)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (group-items
            make-summary
            summary?
            summary-counts
            summary-span
            summary-duration
            summary-failures
            summary-empty
            summary-combine
            summary-concat
            summary-count-strings
            format-duration))

;;; The grammar

(define (split-stretch stretch class)
  "STRETCH, past process and prose items in order, as one group up to its last process
item and singles after it."
  (let ((last-process (list-index (lambda (x) (eq? (class x) 'process))
                                  (reverse stretch))))
    (if (not last-process)
        (map (lambda (x) (cons 'single x)) stretch)
        (let ((cut (- (length stretch) last-process)))
          (cons (cons 'group (list-head stretch cut))
                (map (lambda (x) (cons 'single x)) (list-tail stretch cut)))))))

(define* (group-items items #:key class phase)
  "ITEMS as a list of (single . ITEM) and (group . ITEMS), in order. CLASS maps an
item to process, prose or solo; PHASE maps it to live or past."
  (let loop ((rest items) (stretch '()) (out '()))
    (define (flush) (append (reverse (split-stretch (reverse stretch) class)) out))
    (cond
     ((null? rest) (reverse (flush)))
     ((eq? (phase (car rest)) 'live)
      (append (reverse (flush))
              (map (lambda (x) (cons 'single x)) rest)))
     ((memq (class (car rest)) '(process prose))
      (loop (cdr rest) (cons (car rest) stretch) out))
     (else
      (loop (cdr rest) '() (cons (cons 'single (car rest)) (flush)))))))

;;; The monoid

(define-record-type <summary>
  (%make-summary counts span failures)
  summary?
  ;; An alist of label to count, in the order the labels were first seen.
  (counts   summary-counts)
  ;; (START . END) in milliseconds, or #f when nothing in it was timed.
  (span     summary-span)
  (failures summary-failures))

(define* (make-summary #:optional label #:key (span #f) (failed? #f))
  "The summary of one item: one LABEL, which ran over SPAN, (START . END) in
milliseconds."
  (%make-summary (if label (list (cons label 1)) '())
                 span
                 (if failed? 1 0)))

(define summary-empty (%make-summary '() #f 0))

(define (add-counts a b)
  (fold (lambda (kv acc)
          (if (assoc (car kv) acc)
              (map (lambda (e) (if (equal? (car e) (car kv)) (cons (car e) (+ (cdr e) (cdr kv))) e))
                   acc)
              (append acc (list kv))))
        a b))

(define (summary-combine a b)
  (%make-summary (add-counts (summary-counts a) (summary-counts b))
                 (let ((x (summary-span a)) (y (summary-span b)))
                   (cond ((not x) y)
                         ((not y) x)
                         (else (cons (min (car x) (car y)) (max (cdr x) (cdr y))))))
                 (+ (summary-failures a) (summary-failures b))))

(define (summary-duration s)
  "Milliseconds from the first start to the last end, or #f."
  (let ((span (summary-span s)))
    (and span (- (cdr span) (car span)))))

(define (summary-concat summaries)
  (fold (lambda (s acc) (summary-combine acc s)) summary-empty summaries))

(define* (summary-count-strings s #:optional (nouns '()))
  "\"2 thoughts\", \"1 read\": one string per label. NOUNS is an alist of label to
(SINGULAR PLURAL); a label not in it is used as its own noun, with an s for many."
  (map (lambda (kv)
         (let* ((n (cdr kv))
                (forms (assoc-ref nouns (car kv)))
                (noun (cond
                       (forms (if (= n 1) (car forms) (cadr forms)))
                       ((= n 1) (format #f "~a" (car kv)))
                       (else (format #f "~as" (car kv))))))
           (format #f "~a ~a" n noun)))
       (summary-counts s)))

(define (format-duration ms)
  (let ((s (quotient ms 1000)))
    (if (< s 60)
        (format #f "~as" s)
        (format #f "~am ~as" (quotient s 60) (remainder s 60)))))
