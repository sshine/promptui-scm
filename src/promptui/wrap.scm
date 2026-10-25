;;; Word wrapping by display width.
;;;
;;; Lines break at whitespace when they can and fall back to breaking inside a word
;;; that is wider than the line. The whitespace a line breaks at stays at the end of
;;; that line rather than starting the next one, so a wrapped line's segments
;;; concatenate back to the original and a cursor offset maps onto exactly one of them.
;;;
;;; Every widget that measures text and every widget that draws it uses these, so a
;;; height computed for a layout always matches what is then drawn into it.

(define-module (promptui wrap)
  #:use-module (promptui width)
  #:export (count-wrapped-lines
            wrap-ranges
            wrap-strings
            count-wrapped-text))

(define (wrap-ranges text width)
  "The visual lines of TEXT within WIDTH columns, as (START . END) character offsets.
Always at least one range, so an empty line still occupies a row."
  (let ((len (string-length text)))
    (if (or (<= width 0) (zero? len))
        (list (cons 0 len))
        (let loop ((i 0) (line-start 0) (col 0)
                   (break-at #f) (break-col #f) (acc '()))
          (if (= i len)
              (reverse (cons (cons line-start len) acc))
              (let* ((ch (string-ref text i))
                     (w (char-width ch)))
                (if (and (> (+ col w) width) (> col 0))
                    (if break-at
                        ;; Break where the last whitespace ended; the partial word
                        ;; since then moves down with this character.
                        (let ((col (+ (- col break-col) w)))
                          (loop (+ i 1) break-at col
                                (and (char-whitespace? ch) (+ i 1))
                                (and (char-whitespace? ch) col)
                                (cons (cons line-start break-at) acc)))
                        (loop (+ i 1) i w
                              (and (char-whitespace? ch) (+ i 1))
                              (and (char-whitespace? ch) w)
                              (cons (cons line-start i) acc)))
                    (let ((col (+ col w)))
                      (if (char-whitespace? ch)
                          (loop (+ i 1) line-start col (+ i 1) col acc)
                          (loop (+ i 1) line-start col break-at break-col acc))))))))))

(define (count-wrapped-lines text width)
  "How many visual lines TEXT needs within WIDTH columns. At least one."
  (length (wrap-ranges text width)))

(define (wrap-strings text width)
  "TEXT broken into the strings of its visual lines."
  (map (lambda (r) (substring text (car r) (cdr r)))
       (wrap-ranges text width)))

(define (count-wrapped-text text width)
  "Visual lines for TEXT that may itself contain newlines."
  (apply + (map (lambda (line) (count-wrapped-lines line width))
                (string-split text #\newline))))
