(use-modules (srfi srfi-64)
             (harness)
             (promptui reactive))

(run-suite
 "reactive"
 (lambda ()

   (test-group "a cell's version moves only when its value does"
     (let ((c (make-cell 1)))
       (test-equal 0 (cell-version c))
       (cell-set! c 2)
       (test-equal 1 (cell-version c))
       ;; An equal value is not a change.
       (cell-set! c 2)
       (test-equal 1 (cell-version c))
       (cell-update! c 1+)
       (test-equal 3 (cell-ref c))
       (test-equal 2 (cell-version c))))

   (test-group "track records what was read, and peeking is not reading"
     (let ((a (make-cell 'a))
           (b (make-cell 'b))
           (c (make-cell 'c)))
       (call-with-values
           (lambda () (track (lambda () (cell-peek c) (list (cell-ref a) (cell-ref b) (cell-ref a)))))
         (lambda (result deps)
           (test-equal '(a b a) result)
           (test-equal 2 (length deps))
           (test-assert (assq a deps))
           (test-assert (not (assq c deps)))
           (test-assert (deps-current? deps))
           (cell-set! c 'changed)
           (test-assert (deps-current? deps))
           (cell-set! b 'changed)
           (test-assert (not (deps-current? deps)))))))

   (test-group "a read inside a nested track counts for the outer one too"
     (let ((inner (make-cell 1)))
       (call-with-values
           (lambda ()
             (track (lambda ()
                      (track (lambda () (cell-ref inner)))
                      'outer)))
         (lambda (result deps)
           (test-assert (assq inner deps))))))

   (test-group "reused deps are reported to the render reusing them"
     (let ((c (make-cell 1)))
       (call-with-values
           (lambda () (track (lambda () (cell-ref c))))
         (lambda (_ deps)
           (call-with-values
               (lambda () (track (lambda () (note-deps! deps))))
             (lambda (_ outer) (test-assert (assq c outer))))))))

   (test-group "a raising thunk leaves no frame behind"
     (let ((c (make-cell 1)))
       (test-assert (not (false-if-exception (track (lambda () (error "boom"))))))
       (call-with-values
           (lambda () (track (lambda () #t)))
         (lambda (_ deps) (test-equal '() deps)))))

   (test-group "the clock changes once a second"
     (let ((clock (make-clock 0)))
       (call-with-values
           (lambda () (track (lambda () (clock-seconds clock))))
         (lambda (seconds deps)
           (test-equal 0 seconds)
           (clock-tick! clock 999)
           (test-assert (deps-current? deps))
           (clock-tick! clock 1000)
           (test-assert (not (deps-current? deps)))))))))
