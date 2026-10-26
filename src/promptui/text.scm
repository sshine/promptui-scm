;;; Styled text: spans, lines, and the paragraph that wraps them into an area.
;;;
;;; A span is a string in one style, made with `styled`. A line is a list of spans.
;;; Text is a list of lines. Anywhere text is accepted, a plain string is accepted
;;; too, split at its newlines into unstyled lines, so the common case needs none of
;;; this module.

(define-module (promptui text)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (promptui width)
  #:use-module (promptui wrap)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (styled
            span?
            span-content
            span-style
            line
            line?
            line-spans
            line-plain
            ->text
            text-plain
            text-height
            text-first-line
            wrap-line
            marked-lines
            render-line!
            render-paragraph!))

(define-record-type <span>
  (%span content style)
  span?
  (content span-content)
  (style   span-style))

(define* (styled content #:optional (style %reset-style))
  (%span content style))

(define-record-type <line>
  (%line spans)
  line?
  (spans line-spans))

(define (->span x)
  (cond
   ((span? x) x)
   ((string? x) (styled x))
   (else (styled (format #f "~a" x)))))

(define (line . parts)
  "A line from spans and strings."
  (%line (map ->span parts)))

(define (line-plain l)
  (string-concatenate (map span-content (line-spans l))))

(define (->text x)
  "Normalize X to a list of lines. X is a string, a span, a line, or a list of any
of those, each element of the list being one line."
  (cond
   ((string? x) (map (lambda (s) (%line (list (styled s))))
                     (string-split x #\newline)))
   ((span? x) (list (%line (list x))))
   ((line? x) (list x))
   ((null? x) '())
   ((pair? x) (append-map (lambda (e) (if (line? e) (list e) (->text e))) x))
   (else (->text (format #f "~a" x)))))

(define (text-plain x)
  (string-join (map line-plain (->text x)) "\n"))

(define (text-first-line x)
  (let ((lines (->text x)))
    (if (null? lines) "" (line-plain (car lines)))))

(define (text-height x width)
  "Rows X needs within WIDTH columns once wrapped. Empty text still takes one."
  (let ((lines (->text x)))
    (if (null? lines)
        1
        (apply + (map (lambda (l) (count-wrapped-lines (line-plain l) width))
                      lines)))))

(define (slice-spans spans start end)
  "The spans covering character offsets START to END of their concatenation."
  (let loop ((spans spans) (offset 0) (acc '()))
    (if (or (null? spans) (>= offset end))
        (reverse acc)
        (let* ((s (car spans))
               (len (string-length (span-content s)))
               (from (max start offset))
               (to (min end (+ offset len))))
          (loop (cdr spans) (+ offset len)
                (if (< from to)
                    (cons (styled (substring (span-content s) (- from offset) (- to offset))
                                (span-style s))
                          acc)
                    acc))))))

(define (wrap-line l width)
  "L broken into the lines it occupies within WIDTH columns, keeping its styles."
  (let ((spans (line-spans l)))
    (map (lambda (r) (%line (slice-spans spans (car r) (cdr r))))
         (wrap-ranges (line-plain l) width))))

(define* (marked-lines marker text width #:key (marker-style %reset-style)
                       (style %reset-style))
  "TEXT wrapped into WIDTH columns behind MARKER: the marker before the first line,
and every later line indented to start where the first one did. STYLE is laid
under every span of TEXT, so the lines carry their whole style with them."
  (let* ((indent (string-width marker))
         (room (max 1 (- width indent)))
         (pad (make-string indent #\space))
         (visual (append-map (lambda (l) (wrap-line l room)) (->text text))))
    (map (lambda (l i)
           (%line (cons (if (zero? i) (styled marker marker-style) (styled pad style))
                        (map (lambda (s)
                               (styled (span-content s) (style-patch style (span-style s))))
                             (line-spans l)))))
         (if (null? visual) (list (%line '())) visual)
         (iota (max 1 (length visual))))))

(define* (render-line! buf x y l #:key (style %reset-style) x-max)
  "Draw one line of spans from X,Y, each span patched over STYLE. Returns the column
after the last cell written."
  (fold (lambda (s cx)
          (buffer-set-string! buf cx y (span-content s)
                              (style-patch style (span-style s))
                              x-max))
        x
        (line-spans l)))

(define* (render-paragraph! buf area text #:key (style %reset-style) (wrap? #t)
                            (scroll 0))
  "Draw TEXT into AREA, wrapped at its width unless WRAP? is false, skipping the
first SCROLL visual rows. STYLE is the base every span is patched over, and it is
also laid over the whole area first so that unwritten cells share its background."
  (unless (rect-empty? area)
    (buffer-fill! buf area " " style)
    (let* ((lines (->text text))
           (visual (if wrap?
                       (append-map (lambda (l) (wrap-line l (rect-width area))) lines)
                       lines))
           (shown (if (> scroll 0)
                      (if (> (length visual) scroll) (list-tail visual scroll) '())
                      visual)))
      (let loop ((ls shown) (y (rect-y area)))
        (when (and (pair? ls) (< y (rect-bottom area)))
          (render-line! buf (rect-x area) y (car ls)
                        #:style style #:x-max (rect-right area))
          (loop (cdr ls) (+ y 1))))))
  buf)
