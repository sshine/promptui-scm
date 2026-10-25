(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (harness)
             (promptui width)
             (promptui style)
             (promptui rect)
             (promptui buffer)
             (promptui wrap)
             (promptui text))

(run-suite
 "base"
 (lambda ()

   (test-group "width counts columns, not code points"
     (test-equal 5 (string-width "hello"))
     (test-equal 4 (string-width "日本"))
     (test-equal 2 (string-width "\U01F311"))
     (test-equal 1 (string-width "┃"))
     ;; A combining mark sits on the character before it.
     (test-equal 1 (string-width "é"))
     (test-equal "日" (string-take-width "日本" 3))
     (test-equal "ab   " (string-pad-width "ab" 5)))

   (test-group "word wrap matches promptui-rs"
     (test-equal 1 (count-wrapped-lines "" 10))
     (test-equal 1 (count-wrapped-lines "hello" 10))
     (test-equal 2 (count-wrapped-lines "hello world" 7))
     (test-equal 2 (count-wrapped-lines "abcdefghij" 5))
     (test-equal 3 (count-wrapped-lines "aa bb cc dd" 5))
     (test-equal 1 (count-wrapped-lines "hello" 0))
     (test-equal 1 (count-wrapped-lines "abcde" 5))
     (test-equal 2 (count-wrapped-lines "abcdef" 5))
     (test-equal 2 (count-wrapped-lines "aaa bbb ccc ddd" 9))
     (test-equal 3 (count-wrapped-lines "aaa bbb ccc ddd eee" 8))
     (test-equal '("hello " "world") (wrap-strings "hello world" 7))
     (test-equal '("abcde" "fghij") (wrap-strings "abcdefghij" 5))
     (test-equal '("aa " "bb " "cc dd") (wrap-strings "aa bb cc dd" 5))
     (test-equal '("") (wrap-strings "" 10)))

   (test-group "a wrapped line never exceeds its width and loses nothing"
     (for-each
      (lambda (case)
        (let ((lines (wrap-strings (car case) (cadr case))))
          (test-assert (every (lambda (l) (<= (string-width l) (cadr case))) lines))
          (test-equal (car case) (string-concatenate lines))))
      '(("the quick brown fox jumps over the lazy dog" 10)
        ("abcdef ghijkl mnopqr stuvwx" 8)
        ("a b c d e f g h i j k l m" 3)
        ("aaaa bbbb cccc dddd eeee" 9)
        ("日本語のテキスト" 5))))

   (test-group "patching a style keeps what the patch leaves unset"
     (let* ((base (make-style #:fg 'red #:bg 'blue))
            (over (make-style #:fg 'green #:modifiers '(bold)))
            (s (style-patch base over)))
       (test-equal 'green (style-fg s))
       (test-equal 'blue (style-bg s))
       (test-equal '(bold) (style-modifiers s))))

   (test-group "styles draw as SGR from a reset"
     (test-equal "\x1b[0;39;49m" (style->sgr %reset-style))
     (test-equal "\x1b[0;1;31;100m"
       (style->sgr (make-style #:fg 'red #:bg 'bright-black #:modifiers '(bold))))
     (test-equal "\x1b[0;38;2;250;178;131;48;5;236m"
       (style->sgr (make-style #:fg (rgb 250 178 131) #:bg 236)))
     ;; A typo in a theme draws as the default instead of raising.
     (test-equal "\x1b[0;39;49m" (style->sgr (make-style #:fg 'chartreuse))))

   (test-group "layout"
     (let ((parts (layout-vertical (make-rect 0 0 10 20)
                                   '((min 3) (length 2) (length 5)))))
       (test-equal '(13 2 5) (map rect-height parts))
       (test-equal '(0 13 15) (map rect-y parts)))
     (test-equal '(5 5)
       (map rect-width (layout-horizontal (make-rect 0 0 10 1) '((fill 1) (fill 1)))))
     (test-equal '(3 7)
       (map rect-width (layout-horizontal (make-rect 0 0 10 1) '((max 3) (fill 1)))))
     ;; Too little room: the last parts lose out first.
     (test-equal '(4 0)
       (map rect-height (layout-vertical (make-rect 0 0 1 4) '((length 4) (length 2))))))

   (test-group "rect arithmetic saturates at zero"
     (let ((r (rect-inset (make-rect 0 0 3 3) #:left 2 #:right 2)))
       (test-equal 0 (rect-width r))
       (test-assert (rect-empty? r))))

   (test-group "buffer strings clip, and wide characters take two cells"
     (let ((buf (make-buffer (make-rect 0 0 6 1))))
       (test-equal 6 (buffer-set-string! buf 0 0 "日本語x" %reset-style))
       (test-equal '("日本語") (buffer->lines buf))
       (test-equal "" (buffer-symbol buf 1 0)))
     (let ((buf (make-buffer (make-rect 0 0 5 1))))
       ;; A wide character that would straddle the edge is left out.
       (test-equal 4 (buffer-set-string! buf 0 0 "ab日日" %reset-style))
       (test-equal '("ab日 ") (buffer->lines buf)))
     (let ((buf (make-buffer (make-rect 0 0 6 1))))
       (buffer-set-string! buf 0 0 "日本" %reset-style)
       ;; Overwriting half of a wide character blanks the other half.
       (buffer-set-string! buf 1 0 "x" %reset-style)
       (test-equal '(" x本  ") (buffer->lines buf))))

   (test-group "control characters never reach a cell"
     (let ((buf (make-buffer (make-rect 0 0 10 1))))
       (buffer-set-string! buf 0 0 "a\x1b[31mb\tc" %reset-style)
       (test-equal '("a[31mb  c ") (buffer->lines buf))))

   (test-group "styles patch onto cells"
     (let ((buf (make-buffer (make-rect 0 0 3 1))))
       (buffer-fill! buf (make-rect 0 0 3 1) " " (make-style #:bg 'blue))
       (buffer-set-string! buf 0 0 "ab" (make-style #:fg 'red))
       (test-equal 'blue (style-bg (buffer-style buf 0 0)))
       (test-equal 'red (style-fg (buffer-style buf 0 0)))))

   (test-group "styled text wraps without losing its styles"
     (let* ((red (make-style #:fg 'red))
            (l (line (styled "hello " red) "world"))
            (wrapped (wrap-line l 7)))
       (test-equal '("hello " "world") (map line-plain wrapped))
       (test-equal 'red (style-fg (span-style (car (line-spans (car wrapped))))))
       (test-equal 3 (text-height "a\nhello world" 7))
       (test-equal "a\nb" (text-plain (list "a" (line "b"))))))

   (test-group "a paragraph scrolls by visual rows"
     (test-equal '("world " "      ")
       (render-lines 6 2 (lambda (area buf)
                           (render-paragraph! buf area "hello world" #:scroll 1)))))))
