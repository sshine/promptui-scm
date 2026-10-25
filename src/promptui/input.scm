;;; Keys: decoding what a terminal sends, and naming what was pressed.
;;;
;;; Decoding turns raw characters into key records. A key has a code, which is a
;;; character for anything printable and a symbol otherwise (enter, up, f5, ...), and
;;; a list of modifiers drawn from shift, alt and ctrl. A bracketed paste arrives as
;;; one key whose code is paste and whose text is everything that was pasted.
;;;
;;; Mouse reports, in the SGR encoding a terminal uses once mouse tracking is on,
;;; decode to mouse records instead of keys, in the same list, so one read keeps the
;;; order in which a click and a keystroke happened.
;;;
;;; Both the legacy encodings and the kitty keyboard protocol's CSI u form are
;;; understood. The latter is what makes Shift+Enter and Ctrl+Backspace
;;; distinguishable at all, and a terminal that does not speak it sends the legacy
;;; form instead, so nothing has to be configured.
;;;
;;; Above decoding sit two views of a key. key->descriptor names it in the Emacs style
;;; ("C-c", "M-x", "<up>", "C-<right>"), for keymaps a person writes. key->input maps
;;; it onto the editing actions the widgets understand, as promptui-rs's Input does.

(define-module (promptui input)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-key
            key?
            key-code
            key-modifiers
            key-text
            key-ctrl?
            key-alt?
            key-shift?
            key=?
            make-mouse
            mouse?
            mouse-kind
            mouse-button
            mouse-x
            mouse-y
            mouse-modifiers
            parse-keys
            key->descriptor
            key->input))

(define-immutable-record-type <key>
  (%make-key code modifiers text)
  key?
  (code      key-code)
  (modifiers key-modifiers)
  (text      key-text))

(define* (make-key code #:optional (modifiers '()) #:key (text #f))
  (%make-key code (filter (lambda (m) (memq m modifiers)) '(shift alt ctrl)) text))

(set-record-type-printer! <key>
  (lambda (k port) (format port "#<key ~a>" (key->descriptor k))))

(define (key-ctrl? k) (and (memq 'ctrl (key-modifiers k)) #t))
(define (key-alt? k) (and (memq 'alt (key-modifiers k)) #t))
(define (key-shift? k) (and (memq 'shift (key-modifiers k)) #t))

(define (key=? a b)
  (and (equal? (key-code a) (key-code b))
       (equal? (key-modifiers a) (key-modifiers b))))

;; KIND is press, release, drag, move, scroll-up or scroll-down. BUTTON is left,
;; middle or right, or #f where the report names none. X and Y count from zero.
(define-immutable-record-type <mouse>
  (%make-mouse kind button x y modifiers)
  mouse?
  (kind      mouse-kind)
  (button    mouse-button)
  (x         mouse-x)
  (y         mouse-y)
  (modifiers mouse-modifiers))

(define* (make-mouse kind x y #:key (button #f) (modifiers '()))
  (%make-mouse kind button x y
               (filter (lambda (m) (memq m modifiers)) '(shift alt ctrl))))

(set-record-type-printer! <mouse>
  (lambda (m port)
    (format port "#<mouse ~a~a ~a,~a>"
            (mouse-kind m)
            (if (mouse-button m) (format #f " ~a" (mouse-button m)) "")
            (mouse-x m) (mouse-y m))))

;;; Decoding

(define (modifier-list n)
  "The modifiers a CSI parameter encodes: one more than a bitmask of shift, alt and
ctrl. Meta and super have no counterpart here and are folded into alt."
  (let ((bits (max 0 (- n 1))))
    (append (if (logbit? 0 bits) '(shift) '())
            (if (or (logbit? 1 bits) (logbit? 3 bits) (logbit? 5 bits)) '(alt) '())
            (if (logbit? 2 bits) '(ctrl) '()))))

(define (control-key code)
  "The key a C0 control character stands for."
  (cond
   ((= code 13) (make-key 'enter))
   ((= code 10) (make-key 'enter))
   ((= code 9) (make-key 'tab))
   ((= code 27) (make-key 'escape))
   ((= code 127) (make-key 'backspace))
   ;; Most terminals send ^H for Ctrl+Backspace when Backspace itself is ^?.
   ((= code 8) (make-key 'backspace '(ctrl)))
   ((= code 0) (make-key #\space '(ctrl)))
   ((< code 27) (make-key (integer->char (+ code 96)) '(ctrl)))
   (else (make-key (integer->char (+ code 64)) '(ctrl)))))

(define (char-key ch)
  (let ((code (char->integer ch)))
    (if (or (< code 32) (= code 127))
        (control-key code)
        (make-key ch))))

(define %tilde-keys
  '((1 . home) (2 . insert) (3 . delete) (4 . end) (5 . pageup) (6 . pagedown)
    (7 . home) (8 . end)
    (11 . f1) (12 . f2) (13 . f3) (14 . f4) (15 . f5) (17 . f6) (18 . f7)
    (19 . f8) (20 . f9) (21 . f10) (23 . f11) (24 . f12)))

(define %letter-keys
  '((#\A . up) (#\B . down) (#\C . right) (#\D . left)
    (#\H . home) (#\F . end) (#\E . begin)
    (#\P . f1) (#\Q . f2) (#\R . f3) (#\S . f4)))

;; The kitty protocol reports functional keys as private-use code points, and the
;; ordinary control keys as their ASCII codes.
(define (kitty-key code mods)
  (case code
    ((13) (make-key 'enter mods))
    ((9) (make-key 'tab mods))
    ((27) (make-key 'escape mods))
    ((127) (make-key 'backspace mods))
    ((8) (make-key 'backspace mods))
    (else
     (if (or (< code 32) (and (>= code 57344) (<= code 63743)))
         (make-key 'unknown mods)
         ;; A shifted letter arrives as its unshifted code point with shift set;
         ;; reporting it as the character it types is what a text field wants.
         (let ((ch (integer->char code)))
           (if (and (equal? mods '(shift)) (char-alphabetic? ch))
               (make-key (char-upcase ch))
               (make-key ch mods)))))))

(define (parse-params s)
  "CSI parameters as a list of numbers, a missing one being #f. Sub-parameters after
a colon are dropped."
  (map (lambda (p)
         (let ((main (car (string-split p #\:))))
           (and (not (string-null? main)) (string->number main))))
       (if (string-null? s) '() (string-split s #\;))))

(define (csi-key params final)
  (let* ((ps (parse-params params))
         (p1 (and (pair? ps) (car ps)))
         (mods (if (and (pair? ps) (pair? (cdr ps)) (cadr ps))
                   (modifier-list (cadr ps))
                   '())))
    (cond
     ((char=? final #\~)
      (let ((name (and p1 (assv-ref %tilde-keys p1))))
        (if name (make-key name mods) (make-key 'unknown mods))))
     ((char=? final #\u) (kitty-key (or p1 0) mods))
     ((char=? final #\Z) (make-key 'tab '(shift)))
     ((assv-ref %letter-keys final) => (lambda (name) (make-key name mods)))
     (else (make-key 'unknown mods)))))

(define (sgr-mouse params final)
  "The mouse record for an SGR report, ESC [ < B ; X ; Y and M for a press or m for a
release, or #f when PARAMS is not one."
  (let ((ps (parse-params (substring params 1))))
    (and (= (length ps) 3) (every integer? ps)
         (let* ((b (car ps))
                (x (- (cadr ps) 1))
                (y (- (caddr ps) 1))
                (mods (append (if (logbit? 2 b) '(shift) '())
                              (if (logbit? 3 b) '(alt) '())
                              (if (logbit? 4 b) '(ctrl) '())))
                (low (logand b 3))
                (button (case low ((0) 'left) ((1) 'middle) ((2) 'right) (else #f))))
           (cond
            ((logbit? 6 b)
             ;; Wheel left and right have no use here and are reported as moves.
             (make-mouse (case low ((0) 'scroll-up) ((1) 'scroll-down) (else 'move))
                         x y #:modifiers mods))
            ((logbit? 5 b)
             (make-mouse (if button 'drag 'move) x y #:button button #:modifiers mods))
            ((char=? final #\m)
             (make-mouse 'release x y #:button button #:modifiers mods))
            (else
             (make-mouse 'press x y #:button button #:modifiers mods)))))))

(define %paste-start "\x1b[200~")
(define %paste-end "\x1b[201~")

(define* (parse-keys text #:key (final? #f))
  "Split TEXT into keys and mouse records. Returns two values: those, and the
unconsumed tail.

A tail is held back when it may be the start of a sequence whose remaining bytes
have not arrived yet, so a read that splits a sequence does not turn it into
keystrokes. When FINAL? is true no more input is coming, and a lone escape is
reported as the Escape key rather than held."
  (let ((len (string-length text)))
    (let loop ((i 0) (keys '()))
      (define (done) (values (reverse keys) (substring text i)))
      (cond
       ((>= i len) (values (reverse keys) ""))
       ((string-prefix? %paste-start (substring text i))
        (let ((end (string-contains text %paste-end (+ i (string-length %paste-start)))))
          (if end
              (loop (+ end (string-length %paste-end))
                    (cons (make-key 'paste #:text
                                    (substring text (+ i (string-length %paste-start)) end))
                          keys))
              (done))))
       ((char=? (string-ref text i) #\escape)
        (cond
         ((= (+ i 1) len)
          (if final?
              (loop (+ i 1) (cons (make-key 'escape) keys))
              (done)))
         ((char=? (string-ref text (+ i 1)) #\[)
          ;; CSI: parameters and intermediates, then a final byte in @ to ~.
          (let scan ((j (+ i 2)))
            (cond
             ((>= j len) (if final?
                             (loop len (cons (make-key 'unknown) keys))
                             (done)))
             ((let ((c (char->integer (string-ref text j)))) (and (>= c 64) (<= c 126)))
              (let ((params (substring text (+ i 2) j))
                    (final (string-ref text j)))
                (loop (+ j 1)
                      (cons (cond
                             ((and (string-prefix? "<" params) (memv final '(#\M #\m))
                                   (sgr-mouse params final)))
                             ((and (> (string-length params) 0)
                                   (memv (string-ref params 0) '(#\< #\> #\? #\=)))
                              ;; Private replies are not keys.
                              (make-key 'unknown))
                             (else (csi-key params final)))
                            keys))))
             (else (scan (+ j 1))))))
         ((char=? (string-ref text (+ i 1)) #\O)
          (if (>= (+ i 2) len)
              (if final?
                  (loop len (cons (make-key #\O '(alt)) keys))
                  (done))
              (let ((name (assv-ref %letter-keys (string-ref text (+ i 2)))))
                (loop (+ i 3) (cons (make-key (or name 'unknown)) keys)))))
         (else
          ;; Escape and then a key is that key with Alt held.
          (let* ((k (char-key (string-ref text (+ i 1))))
                 (alt (make-key (key-code k) (cons 'alt (key-modifiers k)))))
            (loop (+ i 2) (cons alt keys))))))
       (else
        (loop (+ i 1) (cons (char-key (string-ref text i)) keys)))))))

;;; Naming

(define (key->descriptor k)
  "The key in Emacs notation: \"a\", \"C-c\", \"M-x\", \"<enter>\", \"C-<right>\"."
  (let* ((code (key-code k))
         (mods (key-modifiers k))
         ;; Shift on a printable character is already in the character.
         (mods (if (char? code) (delete 'shift mods) mods))
         (prefix (string-append (if (memq 'ctrl mods) "C-" "")
                                (if (memq 'alt mods) "M-" "")
                                (if (memq 'shift mods) "S-" ""))))
    (string-append prefix
                   (cond
                    ((char? code) (string code))
                    (else (string-append "<" (symbol->string code) ">"))))))

(define (key->input k)
  "The editing action K stands for, in promptui-rs's terms. A printable character
is returned as itself; everything else is a symbol, and a key with no meaning to the
widgets is unknown."
  (let ((code (key-code k))
        (ctrl (key-ctrl? k))
        (alt (key-alt? k))
        (shift (key-shift? k)))
    (cond
     ((eq? code 'enter) (if (or shift alt) 'newline 'enter))
     ((eq? code 'tab) (if shift 'backtab 'tab))
     ((eq? code 'paste) 'paste)
     ((and ctrl (eqv? code #\c)) 'esc)
     ((and ctrl (eqv? code #\u)) 'clear-all)
     ((and ctrl (eqv? code #\a)) 'ctrl-a)
     ((and ctrl (eqv? code #\w)) 'delete-word-backward)
     ((and (or ctrl alt) (eq? code 'right)) 'word-forward)
     ((and (or ctrl alt) (eq? code 'left)) 'word-backward)
     ((and (or ctrl alt) (eq? code 'backspace)) 'delete-word-backward)
     ;; Only a plain character is text; with Ctrl or Alt it is a command.
     ((char? code) (if (or ctrl alt) 'unknown code))
     ((memq code '(backspace delete left right up down home end pageup pagedown))
      code)
     ((eq? code 'escape) 'esc)
     (else 'unknown))))
