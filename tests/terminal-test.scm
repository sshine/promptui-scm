(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (harness)
             (promptui style)
             (promptui rect)
             (promptui buffer)
             (promptui input)
             (promptui terminal)
             (ice-9 textual-ports))

(define (keys text)
  (call-with-values (lambda () (parse-keys text))
    (lambda (ks rest) (map key->descriptor ks))))

(define (inputs text)
  (call-with-values (lambda () (parse-keys text #:final? #t))
    (lambda (ks rest) (map key->input ks))))

(define (count-substring haystack needle)
  (let loop ((start 0) (n 0))
    (let ((hit (string-contains haystack needle start)))
      (if hit (loop (+ hit (string-length needle)) (+ n 1)) n))))

(define (draw-frames frames)
  "Draw each of FRAMES, a list of lists of row strings, into one terminal and return
what was written for each."
  (let* ((out (open-output-string))
         (t (make-terminal #:input (open-input-string "") #:output out
                           #:size (cons 2 6) #:kitty-keyboard? #f))
         (seen 0))
    (map (lambda (rows)
           (terminal-draw! t (lambda (f)
                               (for-each (lambda (s y)
                                           (buffer-set-string! (frame-buffer f) 0 y s
                                                               %reset-style))
                                         rows (iota (length rows)))))
           (let* ((all (get-output-string out))
                  (written (substring all seen)))
             (set! seen (string-length all))
             written))
         frames)))

(run-suite
 "terminal"
 (lambda ()

   (test-group "printable and control characters"
     (test-equal '("a" "B" "<enter>" "<tab>" "<backspace>" "C-c" "C-<backspace>")
       (keys "aB\r\t\x7f\x03\x08")))

   (test-group "legacy escape sequences"
     (test-equal '("<up>" "<down>" "<right>" "<left>" "<home>" "<end>")
       (keys "\x1b[A\x1b[B\x1bOC\x1b[D\x1b[H\x1b[F"))
     (test-equal '("<pageup>" "<pagedown>" "<delete>" "<f5>")
       (keys "\x1b[5~\x1b[6~\x1b[3~\x1b[15~"))
     (test-equal '("C-<right>" "M-<left>" "S-<up>")
       (keys "\x1b[1;5C\x1b[1;3D\x1b[1;2A"))
     (test-equal '("M-x" "M-<enter>" "S-<tab>")
       (keys "\x1bx\x1b\r\x1b[Z")))

   (test-group "the kitty keyboard protocol"
     (test-equal '("S-<enter>" "C-c" "<escape>" "C-<backspace>" "A")
       (keys "\x1b[13;2u\x1b[99;5u\x1b[27u\x1b[127;5u\x1b[97;2u")))

   (test-group "bracketed paste arrives as one key"
     (call-with-values (lambda () (parse-keys "\x1b[200~line one\nline two\x1b[201~x"))
       (lambda (ks rest)
         (test-equal 2 (length ks))
         (test-equal 'paste (key-code (car ks)))
         (test-equal "line one\nline two" (key-text (car ks))))))

   (test-group "mouse reports decode among the keys"
     (call-with-values
         (lambda () (parse-keys "a\x1b[<0;5;3M\x1b[<32;6;4M\x1b[<0;6;4m\x1b[<65;1;1M\x1b[<20;2;2Mb"))
       (lambda (evs rest)
         (test-equal 7 (length evs))
         (test-equal "a" (key->descriptor (car evs)))
         (let ((press (list-ref evs 1)))
           (test-equal '(press left 4 2)
             (list (mouse-kind press) (mouse-button press) (mouse-x press) (mouse-y press))))
         (test-equal '(drag release scroll-down)
           (map mouse-kind (list (list-ref evs 2) (list-ref evs 3) (list-ref evs 4))))
         (test-equal '(shift ctrl) (mouse-modifiers (list-ref evs 5)))
         (test-equal "b" (key->descriptor (list-ref evs 6)))))
     ;; Half a report waits for the rest, like any other sequence.
     (call-with-values (lambda () (parse-keys "\x1b[<0;5"))
       (lambda (evs rest) (test-equal "\x1b[<0;5" rest))))

   (test-group "mouse reports are switched on and off with the screen"
     (let* ((out (open-output-string))
            (t (make-terminal #:input (open-input-string "") #:output out
                              #:size (cons 2 6) #:kitty-keyboard? #f #:mouse? #t)))
       (terminal-enter! t)
       (test-assert (string-contains (get-output-string out) "\x1b[?1006h"))
       (terminal-set-mouse! t #f)
       (test-assert (string-contains (get-output-string out) "\x1b[?1000l"))
       (terminal-leave! t)
       ;; Already off, so leaving does not switch it off a second time.
       (test-equal 1 (count-substring (get-output-string out) "\x1b[?1000l"))))

   (test-group "a sequence split across reads is held back"
     (call-with-values (lambda () (parse-keys "a\x1b[1;"))
       (lambda (ks rest)
         (test-equal 1 (length ks))
         (test-equal "\x1b[1;" rest)))
     (call-with-values (lambda () (parse-keys "\x1b"))
       (lambda (ks rest) (test-equal "\x1b" rest)))
     ;; Unless no more input is coming, when a lone escape is the Escape key.
     (call-with-values (lambda () (parse-keys "\x1b" #:final? #t))
       (lambda (ks rest) (test-equal '("<escape>") (map key->descriptor ks)))))

   (test-group "keys map onto editing actions as promptui-rs's Input does"
     (test-equal '(enter newline newline) (inputs "\r\x1b[13;2u\x1b\r"))
     (test-equal '(esc clear-all ctrl-a) (inputs "\x03\x15\x01"))
     (test-equal '(word-forward word-backward delete-word-backward)
       (inputs "\x1b[1;5C\x1b[1;5D\x1b[127;5u"))
     (test-equal (list #\a 'unknown 'unknown) (inputs "a\x18\x1bx"))
     (test-equal '(esc) (inputs "\x1b")))

   (test-group "the first frame is drawn in full, inside a synchronized pair"
     (let ((out (car (draw-frames '(("ab" "cd"))))))
       (test-assert (string-contains out "\x1b[?2026h"))
       (test-assert (string-contains out "\x1b[?2026l"))
       (test-assert (string-contains out "\x1b[?1049h"))
       (test-assert (string-contains out "ab"))
       (test-assert (string-contains out "cd"))))

   (test-group "only the cells that changed are written again"
     (let ((outs (draw-frames '(("ab" "cd") ("ab" "cX") ("ab" "cX")))))
       (test-assert (string-contains (cadr outs) "\x1b[2;2H\x1b[0;39;49mX"))
       (test-assert (not (string-contains (cadr outs) "\x1b[1;1H")))
       ;; An unchanged frame writes no cells at all.
       (test-equal 0 (count-substring (caddr outs) "H"))))

   (test-group "a frame places the cursor where it was asked"
     (let* ((out (open-output-string))
            (t (make-terminal #:input (open-input-string "") #:output out
                              #:size (cons 3 10) #:kitty-keyboard? #f)))
       (terminal-draw! t (lambda (f) (frame-set-cursor! f 4 2)))
       (test-assert (string-contains (get-output-string out) "\x1b[3;5H\x1b[?25h"))))

   (test-group "reading keys from a port"
     (let ((t (make-terminal #:input (open-input-string "hi\x1b[A")
                             #:output (open-output-string)
                             #:size (cons 3 10) #:kitty-keyboard? #f)))
       (test-equal '("h" "i" "<up>") (map key->descriptor (terminal-read-keys t 0)))))

   (test-group "the diff rewrites a wide character whole"
     (let* ((a (make-buffer (make-rect 0 0 4 1)))
            (b (make-buffer (make-rect 0 0 4 1))))
       (buffer-set-string! a 0 0 "日本" %reset-style)
       (buffer-set-string! b 0 0 "日x" %reset-style)
       (let ((out (render-diff a b)))
         (test-assert (string-contains out "\x1b[1;3H"))
         (test-assert (string-contains out "x")))))))
