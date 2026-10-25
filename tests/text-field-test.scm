(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (harness)
             (promptui style)
             (promptui rect)
             (promptui buffer)
             (promptui theme)
             (promptui padding)
             (promptui widgets text-field))

(define (tf) (make-text-field %opencode-theme))

(define (type! field text)
  (string-for-each (lambda (ch)
                     (text-field-input! field (if (char=? ch #\newline) 'newline ch)))
                   text)
  field)

(define (tf-with-width text width)
  "A field holding TEXT with no padding, measured at WIDTH so that movement by
visual line knows the wrap width."
  (let ((field (type! (tf) text)))
    (text-field-set-padding! field (padding-uniform 0))
    (text-field-required-height field width)
    field))

(run-suite
 "text-field"
 (lambda ()

   (test-group "a new field is empty"
     (test-equal "" (text-field-content (tf))))

   (test-group "typing and submitting"
     (let ((field (type! (tf) "hi")))
       (test-equal "hi" (text-field-content field))
       (test-equal '(submit . "hi") (text-field-input! field 'enter))
       (test-equal "" (text-field-content field))))

   (test-group "newline and backspace"
     (test-equal "a\nb" (text-field-content (type! (tf) "a\nb")))
     (let ((field (type! (tf) "ab")))
       (text-field-input! field 'backspace)
       (test-equal "a" (text-field-content field)))
     (let ((field (type! (tf) "a\n")))
       (text-field-input! field 'backspace)
       (test-equal "a" (text-field-content field))))

   (test-group "empty enter is ignored"
     (test-equal 'ignored (text-field-input! (tf) 'enter))
     (test-equal 'ignored (text-field-input! (type! (tf) "   ") 'enter)))

   (test-group "clear all"
     (let ((field (type! (tf) "hello")))
       (test-equal 'edited (text-field-input! field 'clear-all))
       (test-equal "" (text-field-content field))
       (test-equal '(0 . 0) (text-field-cursor field)))
     (test-equal 'ignored (text-field-input! (tf) 'clear-all))
     (let ((field (type! (tf) "a\nb")))
       (text-field-input! field 'clear-all)
       (test-equal "" (text-field-content field))))

   (test-group "height follows content between the minimum and the maximum"
     ;; min 3, plus padding 1 above and below
     (test-equal 5 (text-field-required-height (tf) 80))
     (test-equal 7 (text-field-required-height (type! (tf) "x\nx\nx\nx\nx") 80))
     (test-equal 10 (text-field-required-height (type! (tf) "x\nx\nx\nx\nx\nx\nx\nx\nx\nx\nx") 80))
     (let ((field (tf)))
       (text-field-set-padding! field (padding-symmetric 2 3))
       (test-equal 9 (text-field-required-height field 80))))

   (test-group "the field grows before the cursor reaches the edge"
     (let ((field (type! (tf) "abcdefghi")))
       (text-field-set-padding! field (padding-uniform 0))
       (test-equal 3 (text-field-required-height field 12))
       (type! field "j")
       (test-equal 3 (text-field-required-height field 12))))

   (test-group "scrolling keeps the cursor visible"
     (let ((field (type! (tf) "a\nb\nc\nd\ne")))
       (text-field-set-max-height! field 3)
       (text-field-set-padding! field (padding-uniform 0))
       (test-equal 3 (text-field-required-height field 80))
       ;; The cursor is on the fifth line, so the view shows lines three to five.
       (test-equal '("┃c" "┃d" "┃e")
         (map (lambda (s) (string-trim-right (substring s 0 2)))
              (render-lines 80 3 (lambda (area buf) (text-field-render! field area buf)))))))

   (test-group "up and down move between logical lines"
     (let ((field (tf-with-width "aaa\nbbb\nccc" 80)))
       (text-field-set-cursor! field 2 2)
       (text-field-input! field 'up)
       (test-equal '(1 . 2) (text-field-cursor field))
       (text-field-input! field 'up)
       (test-equal '(0 . 2) (text-field-cursor field))
       (text-field-input! field 'down)
       (test-equal '(1 . 2) (text-field-cursor field))))

   (test-group "up and down move across wrapped visual lines"
     ;; Width 6 leaves 4 columns after the bar and the scrollbar, and 3 to wrap at
     ;; after the cursor's reserved column: "abc" "def".
     (let ((field (tf-with-width "abcdef" 6)))
       (text-field-set-cursor! field 0 1)
       (text-field-input! field 'down)
       (test-equal '(0 . 4) (text-field-cursor field)))
     (let ((field (tf-with-width "abcdef" 6)))
       (test-equal '(0 . 6) (text-field-cursor field))
       (text-field-input! field 'up)
       (test-assert (<= (cdr (text-field-cursor field)) 3))))

   (test-group "the cursor is drawn once at a wrap boundary"
     (let ((field (tf-with-width "abcdef" 6))
           (buf (make-buffer (make-rect 0 0 6 3))))
       (text-field-set-cursor! field 0 3)
       (text-field-render! field (make-rect 0 0 6 3) buf)
       (test-equal 1 (count-cells buf (lambda (sym style)
                                        (style-has-modifier? style 'reverse))))))

   (test-group "word movement and deletion"
     (let ((field (type! (tf) "hello brave world")))
       (text-field-input! field 'word-backward)
       (test-equal '(0 . 12) (text-field-cursor field))
       (text-field-input! field 'word-backward)
       (test-equal '(0 . 6) (text-field-cursor field))
       (text-field-input! field 'word-forward)
       (test-equal '(0 . 12) (text-field-cursor field))
       (text-field-input! field 'end)
       (text-field-input! field 'delete-word-backward)
       (test-equal "hello brave " (text-field-content field))))

   (test-group "a paste keeps its newlines"
     (let ((field (tf)))
       (text-field-input! field "one\r\ntwo")
       (test-equal "one\ntwo" (text-field-content field))
       (test-equal '(1 . 3) (text-field-cursor field))))

   (test-group "the cursor position accounts for the bar and the padding"
     (let ((field (type! (tf) "ab")))
       (text-field-required-height field 20)
       (test-equal '(4 . 1) (text-field-cursor-position field (make-rect 0 0 20 5)))
       (text-field-set-focused! field #f)
       (test-equal #f (text-field-cursor-position field (make-rect 0 0 20 5)))))

   (test-group "a style procedure colours individual characters"
     (let ((field (type! (tf) "$ ls"))
           (buf (make-buffer (make-rect 0 0 20 5))))
       (text-field-set-style-proc! field
                                   (lambda (row col ch)
                                     (and (= row 0) (= col 0) (char=? ch #\$)
                                          (make-style #:fg 'yellow))))
       (text-field-required-height field 20)
       (text-field-render! field (make-rect 0 0 20 5) buf)
       (test-equal 'yellow (style-fg (buffer-style buf 2 1)))
       (test-equal (theme-text %opencode-theme) (style-fg (buffer-style buf 4 1)))))))
