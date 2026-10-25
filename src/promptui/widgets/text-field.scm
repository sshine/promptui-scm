;;; A word-wrapping, multi-line text input that grows with its content.
;;;
;;; The field owns its text and cursor. Enter submits; Shift+Enter or Alt+Enter
;;; inserts a newline. It grows from a minimum to a maximum height as the text wraps
;;; and scrolls once it reaches the maximum, always keeping the cursor in view.
;;;
;;; One column is reserved past the end of every line for the cursor, so a cursor at
;;; the end of a full line sits beside the text instead of on the next row.
;;;
;;; Text is a list of logical lines, and the cursor is a logical line and a character
;;; offset into it. Visual lines, the ones wrapping produces, are computed on demand.

(define-module (promptui widgets text-field)
  #:use-module (promptui width)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (promptui wrap)
  #:use-module (promptui theme)
  #:use-module (promptui padding)
  #:use-module (promptui widgets common)
  #:use-module (promptui widgets scrollbar)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-text-field
            text-field?
            text-field-theme
            text-field-content
            text-field-set-content!
            text-field-clear!
            text-field-empty?
            text-field-cursor
            text-field-set-cursor!
            text-field-focused?
            text-field-set-focused!
            text-field-padding
            text-field-set-padding!
            text-field-set-border-color!
            text-field-set-min-height!
            text-field-set-max-height!
            text-field-set-summary-mode!
            text-field-set-style-proc!
            text-field-required-height
            text-field-cursor-position
            text-field-input!
            text-field-render!))

(define %scrollbar-width 1)
(define %cursor-reserve 1)

(define-record-type <text-field>
  (%make-text-field lines cursor theme padding border-color min-height max-height
                    focused scroll content-width style-proc summary)
  text-field?
  (lines         tf-lines set-tf-lines!)
  ;; (ROW . COLUMN): a logical line and a character offset into it.
  (cursor        text-field-cursor set-tf-cursor!)
  (theme         text-field-theme)
  (padding       text-field-padding text-field-set-padding!)
  (border-color  tf-border-color text-field-set-border-color!)
  (min-height    tf-min-height text-field-set-min-height!)
  (max-height    tf-max-height text-field-set-max-height!)
  (focused       text-field-focused? text-field-set-focused!)
  ;; Visual lines scrolled off the top.
  (scroll        tf-scroll set-tf-scroll!)
  ;; The width the field was last measured at, which cursor movement by visual
  ;; line needs between frames.
  (content-width tf-content-width set-tf-content-width!)
  ;; (lambda (row column char) style-or-#f), patched over the text style.
  (style-proc    tf-style-proc text-field-set-style-proc!)
  ;; When true the field keeps its minimum height whatever it holds.
  (summary       tf-summary text-field-set-summary-mode!))

(define* (make-text-field theme #:key
                          (padding (padding-uniform 1))
                          (border-color (theme-primary theme))
                          (min-height 3)
                          (max-height 8)
                          (focused #t))
  (%make-text-field (list "") (cons 0 0) theme padding border-color
                    min-height max-height focused 0 0 #f #f))

(define (text-field-content tf)
  "The text, logical lines joined with newlines."
  (string-join (tf-lines tf) "\n"))

(define (text-field-set-content! tf text)
  "Replace the text, with the cursor at its end."
  (let ((lines (string-split text #\newline)))
    (set-tf-lines! tf lines)
    (set-tf-cursor! tf (cons (- (length lines) 1)
                             (string-length (last lines))))
    (set-tf-scroll! tf 0)))

(define (text-field-clear! tf)
  (set-tf-lines! tf (list ""))
  (set-tf-cursor! tf (cons 0 0))
  (set-tf-scroll! tf 0))

(define (text-field-empty? tf)
  (equal? (tf-lines tf) '("")))

(define (row tf) (car (text-field-cursor tf)))
(define (col tf) (cdr (text-field-cursor tf)))
(define (current-line tf) (list-ref (tf-lines tf) (row tf)))

(define (h-overhead tf)
  (+ %border-width (padding-horizontal (text-field-padding tf)) %scrollbar-width))

(define (wrap-width tf)
  (max 0 (- (tf-content-width tf) %cursor-reserve)))

;;; Measuring

(define (total-visual-lines tf width)
  (apply + (map (lambda (l) (count-wrapped-lines l width)) (tf-lines tf))))

(define (text-field-required-height tf width)
  "The height the field wants at WIDTH, padding included. Measuring also remembers
the width and scrolls to keep the cursor visible, so call it before rendering."
  (let ((content-width (max 0 (- width (h-overhead tf)))))
    (set-tf-content-width! tf content-width)
    (let ((pad (padding-vertical (text-field-padding tf)))
          (ww (wrap-width tf)))
      (cond
       ((tf-summary tf) (+ (tf-min-height tf) pad))
       ((zero? ww) (tf-min-height tf))
       (else
        (let ((h (max (tf-min-height tf)
                      (min (tf-max-height tf) (total-visual-lines tf ww)))))
          (ensure-cursor-visible! tf ww h)
          (+ h pad)))))))

(define (segment-index ranges column)
  "Which of RANGES a cursor at COLUMN belongs to. At a wrap boundary it belongs to
the start of the next segment; past the end, to the last."
  (let loop ((rs ranges) (i 0))
    (cond
     ((null? (cdr rs)) i)
     ((< column (cdar rs)) i)
     (else (loop (cdr rs) (+ i 1))))))

(define (cursor-visual-position tf width)
  "(VISUAL-LINE . DISPLAY-COLUMN) of the cursor."
  (let loop ((lines (tf-lines tf)) (i 0) (visual 0))
    (cond
     ((null? lines) (cons visual 0))
     ((= i (row tf))
      (let* ((line (car lines))
             (ranges (wrap-ranges line width))
             (seg (segment-index ranges (col tf)))
             (start (car (list-ref ranges seg))))
        (cons (+ visual seg)
              (string-width (substring line start (max start (col tf)))))))
     (else (loop (cdr lines) (+ i 1)
                 (+ visual (count-wrapped-lines (car lines) width)))))))

(define (visual->cursor tf target column width)
  "The logical cursor on visual line TARGET nearest to display COLUMN."
  (let loop ((lines (tf-lines tf)) (i 0) (visual 0))
    (if (null? lines)
        (cons (- (length (tf-lines tf)) 1) (string-length (last (tf-lines tf))))
        (let* ((line (car lines))
               (ranges (wrap-ranges line width))
               (n (length ranges)))
          (if (< target (+ visual n))
              (let* ((r (list-ref ranges (- target visual)))
                     (segment (substring line (car r) (cdr r))))
                (cons i (+ (car r)
                           (let walk ((j 0) (used 0))
                             (if (= j (string-length segment))
                                 j
                                 (let ((w (char-width (string-ref segment j))))
                                   (if (> (+ used w) column)
                                       j
                                       (walk (+ j 1) (+ used w)))))))))
              (loop (cdr lines) (+ i 1) (+ visual n)))))))

(define (ensure-cursor-visible! tf width height)
  (when (> height 0)
    (let ((line (car (cursor-visual-position tf width))))
      (when (< line (tf-scroll tf))
        (set-tf-scroll! tf line))
      (when (>= line (+ (tf-scroll tf) height))
        (set-tf-scroll! tf (+ 1 (- line height)))))))

(define (text-field-cursor-position tf area)
  "Where the terminal cursor belongs as (X . Y) when the field is drawn in AREA, or
#f when it is unfocused or the cursor is out of view."
  (let ((ww (wrap-width tf))
        (pad (text-field-padding tf)))
    (and (text-field-focused? tf)
         (> ww 0)
         (let* ((pos (cursor-visual-position tf ww))
                (x (+ (rect-x area) %border-width (padding-left pad) (cdr pos)))
                (y (+ (rect-y area) (padding-top pad) (max 0 (- (car pos) (tf-scroll tf))))))
           (and (< y (rect-bottom area))
                (< x (rect-right area))
                (cons x y))))))

;;; Editing

(define (text-field-input! tf input)
  "Apply one input, a character or an action symbol from key->input. Returns
edited, ignored, or (submit . TEXT) when Enter was pressed on non-blank text, in
which case the field is cleared."
  (cond
   ((char? input) (insert-string! tf (string input)) 'edited)
   ((string? input) (insert-string! tf input) 'edited)
   (else
    (case input
      ((enter)
       (let ((text (text-field-content tf)))
         (if (string-null? (string-trim-both text))
             'ignored
             (begin (text-field-clear! tf) (cons 'submit text)))))
      ((newline) (insert-newline! tf) 'edited)
      ((backspace) (if (delete-backward! tf) 'edited 'ignored))
      ((delete) (if (delete-forward! tf) 'edited 'ignored))
      ((delete-word-backward) (if (delete-word-backward! tf) 'edited 'ignored))
      ((clear-all)
       (if (text-field-empty? tf) 'ignored (begin (text-field-clear! tf) 'edited)))
      ((left) (move-left! tf) 'edited)
      ((right) (move-right! tf) 'edited)
      ((up) (move-vertically! tf -1) 'edited)
      ((down) (move-vertically! tf 1) 'edited)
      ((home) (set-tf-cursor! tf (cons (row tf) 0)) 'edited)
      ((end) (set-tf-cursor! tf (cons (row tf) (string-length (current-line tf)))) 'edited)
      ((word-forward) (move-word-forward! tf) 'edited)
      ((word-backward) (move-word-backward! tf) 'edited)
      (else 'ignored)))))

(define (replace-line! tf i new)
  (set-tf-lines! tf (append (list-head (tf-lines tf) i)
                            (list new)
                            (list-tail (tf-lines tf) (+ i 1)))))

(define (insert-string! tf s)
  "Insert S at the cursor. Newlines in it split the line, so a paste arrives intact."
  (let ((parts (string-split s #\newline)))
    (let loop ((parts parts) (first? #t))
      (unless (null? parts)
        (unless first? (insert-newline! tf))
        (let* ((line (current-line tf))
               (c (col tf))
               (piece (string-filter (lambda (ch) (not (char=? ch #\return)))
                                     (car parts))))
          (replace-line! tf (row tf)
                         (string-append (substring line 0 c) piece (substring line c)))
          (set-tf-cursor! tf (cons (row tf) (+ c (string-length piece)))))
        (loop (cdr parts) #f)))))

(define (insert-newline! tf)
  (let* ((r (row tf))
         (line (current-line tf))
         (c (col tf)))
    (set-tf-lines! tf (append (list-head (tf-lines tf) r)
                              (list (substring line 0 c) (substring line c))
                              (list-tail (tf-lines tf) (+ r 1))))
    (set-tf-cursor! tf (cons (+ r 1) 0))))

(define (join-with-next! tf r)
  "Append line R+1 to line R."
  (let ((lines (tf-lines tf)))
    (set-tf-lines! tf (append (list-head lines r)
                              (list (string-append (list-ref lines r)
                                                   (list-ref lines (+ r 1))))
                              (list-tail lines (+ r 2))))))

(define (delete-backward! tf)
  (let ((r (row tf)) (c (col tf)))
    (cond
     ((> c 0)
      (let ((line (current-line tf)))
        (replace-line! tf r (string-append (substring line 0 (- c 1)) (substring line c)))
        (set-tf-cursor! tf (cons r (- c 1)))
        #t))
     ((> r 0)
      (let ((prev-len (string-length (list-ref (tf-lines tf) (- r 1)))))
        (join-with-next! tf (- r 1))
        (set-tf-cursor! tf (cons (- r 1) prev-len))
        #t))
     (else #f))))

(define (delete-forward! tf)
  (let* ((r (row tf)) (c (col tf)) (line (current-line tf)))
    (cond
     ((< c (string-length line))
      (replace-line! tf r (string-append (substring line 0 c) (substring line (+ c 1))))
      #t)
     ((< (+ r 1) (length (tf-lines tf)))
      (join-with-next! tf r)
      #t)
     (else #f))))

(define (word-char? ch)
  (or (char-alphabetic? ch) (char-numeric? ch)))

(define (word-start-backward line c)
  "Back over anything that is not a word character, then over the word before it."
  (let* ((i (let skip ((i c))
              (if (and (> i 0) (not (word-char? (string-ref line (- i 1)))))
                  (skip (- i 1))
                  i))))
    (let skip ((i i))
      (if (and (> i 0) (word-char? (string-ref line (- i 1))))
          (skip (- i 1))
          i))))

(define (word-end-forward line c)
  "Over the rest of this word, then over whatever separates it from the next."
  (let* ((len (string-length line))
         (i (let skip ((i c))
              (if (and (< i len) (word-char? (string-ref line i)))
                  (skip (+ i 1))
                  i))))
    (let skip ((i i))
      (if (and (< i len) (not (word-char? (string-ref line i))))
          (skip (+ i 1))
          i))))

(define (delete-word-backward! tf)
  (let ((r (row tf)) (c (col tf)))
    (if (zero? c)
        (delete-backward! tf)
        (let* ((line (current-line tf))
               (start (word-start-backward line c)))
          (replace-line! tf r (string-append (substring line 0 start) (substring line c)))
          (set-tf-cursor! tf (cons r start))
          #t))))

(define (move-left! tf)
  (let ((r (row tf)) (c (col tf)))
    (cond
     ((> c 0) (set-tf-cursor! tf (cons r (- c 1))))
     ((> r 0) (set-tf-cursor! tf (cons (- r 1)
                                       (string-length (list-ref (tf-lines tf) (- r 1)))))))))

(define (move-right! tf)
  (let ((r (row tf)) (c (col tf)))
    (cond
     ((< c (string-length (current-line tf))) (set-tf-cursor! tf (cons r (+ c 1))))
     ((< (+ r 1) (length (tf-lines tf))) (set-tf-cursor! tf (cons (+ r 1) 0))))))

(define (move-vertically! tf delta)
  "One visual line up or down, keeping the display column where it can."
  (let ((ww (wrap-width tf)))
    (when (> ww 0)
      (let* ((pos (cursor-visual-position tf ww))
             (target (+ (car pos) delta)))
        (when (and (>= target 0) (< target (total-visual-lines tf ww)))
          (set-tf-cursor! tf (visual->cursor tf target (cdr pos) ww)))))))

(define (move-word-forward! tf)
  (let ((r (row tf)) (c (col tf)) (line (current-line tf)))
    (if (>= c (string-length line))
        (when (< (+ r 1) (length (tf-lines tf)))
          (set-tf-cursor! tf (cons (+ r 1) 0)))
        (set-tf-cursor! tf (cons r (word-end-forward line c))))))

(define (move-word-backward! tf)
  (let ((r (row tf)) (c (col tf)))
    (if (zero? c)
        (when (> r 0)
          (set-tf-cursor! tf (cons (- r 1)
                                   (string-length (list-ref (tf-lines tf) (- r 1))))))
        (set-tf-cursor! tf (cons r (word-start-backward (current-line tf) c))))))

;;; Drawing

(define (text-field-render! tf area buf)
  (unless (rect-empty? area)
    (let* ((theme (text-field-theme tf))
           (pad (text-field-padding tf))
           (ww (wrap-width tf))
           (text-style (theme-input-style theme))
           (border-style (make-style #:fg (if (text-field-focused? tf)
                                              (tf-border-color tf)
                                              (theme-border theme))
                                     #:bg (theme-element-bg theme)))
           (text-x (+ (rect-x area) %border-width (padding-left pad)))
           (text-y (+ (rect-y area) (padding-top pad)))
           (visible-rows (max 0 (- (rect-height area) (padding-vertical pad))))
           (x-limit (- (rect-right area) (padding-right pad) %scrollbar-width))
           (scroll (tf-scroll tf))
           (cursor (text-field-cursor tf))
           (style-proc (tf-style-proc tf)))
      (fill-background! buf area text-style)
      (draw-left-bar! buf area border-style)
      (let loop ((lines (tf-lines tf)) (li 0) (visual 0))
        (unless (null? lines)
          (let* ((line (car lines))
                 (ranges (wrap-ranges line ww))
                 (last-seg (- (length ranges) 1)))
            (for-each
             (lambda (r seg)
               (let ((v (+ visual seg)))
                 (when (and (>= v scroll) (< v (+ scroll visible-rows)))
                   (let ((y (+ text-y (- v scroll))))
                     (let draw ((i (car r)) (x text-x))
                       (cond
                        ((< i (cdr r))
                         (let* ((ch (string-ref line i))
                                (w (char-width ch)))
                           (if (> (+ x w) x-limit)
                               (draw (cdr r) x)
                               (let* ((base (let ((over (and style-proc (style-proc li i ch))))
                                              (if over (style-patch text-style over) text-style)))
                                      (style (if (and (text-field-focused? tf)
                                                      (= li (car cursor))
                                                      (= i (cdr cursor)))
                                                 (style-add-modifier base 'reverse)
                                                 base)))
                                 (draw (+ i 1) (buffer-set-string! buf x y (string ch) style x-limit))))))
                        ;; The cursor past the end of a line is drawn after its last
                        ;; segment only; at a wrap boundary it belongs to the next.
                        ((and (text-field-focused? tf)
                              (= li (car cursor))
                              (= (cdr cursor) (cdr r))
                              (= seg last-seg)
                              (< x x-limit))
                         (buffer-set-cell! buf x y " "
                                           (style-add-modifier text-style 'reverse)))))))))
             ranges (iota (length ranges)))
            (loop (cdr lines) (+ li 1) (+ visual (length ranges))))))
      (let ((sb (make-scrollbar (total-visual-lines tf ww) visible-rows scroll)))
        (when (scrollbar-visible? sb)
          (scrollbar-render! sb
                             (make-rect (- (rect-right area) (padding-right pad) %scrollbar-width)
                                        text-y %scrollbar-width visible-rows)
                             buf)))))
  buf)

(define (text-field-set-cursor! tf row column)
  "Put the cursor at character COLUMN of logical line ROW, clamped to the text."
  (let* ((lines (tf-lines tf))
         (r (max 0 (min row (- (length lines) 1)))))
    (set-tf-cursor! tf (cons r (max 0 (min column (string-length (list-ref lines r))))))))
