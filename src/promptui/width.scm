;;; Display width: how many terminal columns a character or a string occupies.
;;;
;;; An explicit table rather than libc's wcwidth. wcwidth consults the locale, so in
;;; the C locale, which is what a Nix build sandbox and many CI runners provide, it
;;; reports every non-ASCII character as one column. A cell buffer is only correct if
;;; every string is exactly as wide as it claims, so that cannot depend on the
;;; environment.
;;;
;;; These are the East Asian Wide and Fullwidth ranges, plus the emoji blocks that are
;;; wide in every terminal worth supporting. Grapheme clustering is out of scope: a
;;; ZWJ sequence is measured as its parts.

(define-module (promptui width)
  #:use-module (srfi srfi-1)
  #:export (char-width
            string-width
            string-take-width
            string-pad-width))

(define %wide-ranges
  '((#x1100 . #x115F)     ; Hangul Jamo
    (#x231A . #x231B)     ; watch, hourglass
    (#x23E9 . #x23EC)
    (#x23F0 . #x23F0)
    (#x23F3 . #x23F3)
    (#x25FD . #x25FE)
    (#x2614 . #x2615)
    (#x2648 . #x2653)
    (#x26AA . #x26AB)
    (#x26BD . #x26BE)
    (#x26C4 . #x26C5)
    (#x26CE . #x26CE)
    (#x26D4 . #x26D4)
    (#x26EA . #x26EA)
    (#x26F2 . #x26F5)
    (#x26FA . #x26FD)
    (#x2705 . #x2705)
    (#x270A . #x270B)
    (#x2728 . #x2728)
    (#x274C . #x274C)
    (#x2753 . #x2755)
    (#x2757 . #x2757)
    (#x2795 . #x2797)
    (#x27B0 . #x27B0)
    (#x27BF . #x27BF)
    (#x2B1B . #x2B1C)
    (#x2B50 . #x2B50)
    (#x2B55 . #x2B55)
    (#x2E80 . #x303E)     ; CJK radicals, Kangxi, CJK symbols
    (#x3041 . #x33FF)     ; Hiragana, Katakana, Bopomofo, compatibility
    (#x3400 . #x4DBF)     ; CJK extension A
    (#x4E00 . #x9FFF)     ; CJK unified ideographs
    (#xA000 . #xA4CF)     ; Yi
    (#xAC00 . #xD7A3)     ; Hangul syllables
    (#xF900 . #xFAFF)     ; CJK compatibility ideographs
    (#xFE10 . #xFE19)     ; vertical forms
    (#xFE30 . #xFE6F)     ; CJK compatibility forms
    (#xFF00 . #xFF60)     ; fullwidth forms
    (#xFFE0 . #xFFE6)     ; fullwidth signs
    (#x1F300 . #x1F64F)   ; symbols, pictographs, emoticons
    (#x1F680 . #x1F6FF)   ; transport and map symbols
    (#x1F900 . #x1F9FF)   ; supplemental symbols and pictographs
    (#x20000 . #x2FFFD)   ; CJK extension B and beyond
    (#x30000 . #x3FFFD)))

;; Combining marks sit on the previous character and take no column of their own.
(define %zero-ranges
  '((#x0300 . #x036F)     ; combining diacriticals
    (#x0483 . #x0489)
    (#x0591 . #x05BD)
    (#x0610 . #x061A)
    (#x064B . #x065F)
    (#x200B . #x200F)     ; zero-width spaces and marks
    (#x20D0 . #x20F0)     ; combining marks for symbols
    (#xFE00 . #xFE0F)     ; variation selectors
    (#xFE20 . #xFE2F)))

(define (in-ranges? code ranges)
  (any (lambda (r) (and (>= code (car r)) (<= code (cdr r)))) ranges))

(define (char-width ch)
  "Columns CH occupies. Control characters and combining marks count as zero."
  (let ((code (char->integer ch)))
    (cond
     ((< code 32) 0)
     ((< code 127) 1)
     ((< code 160) 0)
     ((in-ranges? code %zero-ranges) 0)
     ((in-ranges? code %wide-ranges) 2)
     (else 1))))

(define (string-width s)
  "Columns S occupies."
  (string-fold (lambda (ch total) (+ total (char-width ch))) 0 s))

(define (string-take-width s width)
  "The longest prefix of S that fits in WIDTH columns. A wide character that would
straddle the edge is left out rather than cut in half."
  (let ((len (string-length s)))
    (let loop ((i 0) (used 0))
      (if (= i len)
          s
          (let ((w (char-width (string-ref s i))))
            (if (> (+ used w) width)
                (substring s 0 i)
                (loop (+ i 1) (+ used w))))))))

(define (string-pad-width s width)
  "S cut or padded with spaces to exactly WIDTH columns."
  (let* ((cut (string-take-width s width))
         (short (- width (string-width cut))))
    (if (> short 0)
        (string-append cut (make-string short #\space))
        cut)))
