;;; A conversation: content blocks stacked bottom-up, each marked by a coloured bar.
;;; A message can carry a marker instead, such as a bullet, drawn before its first
;;; line with the rest of the text indented to line up after it.
;;;
;;; A block is a chat message, a tool call, a questionnaire summary or a todo list.
;;; Each can be drawn in full or compact, as one line; the list has a mode for all
;;; of them, and a block can override it on its own.
;;;
;;; The list does not scroll itself. message-list-scrollable wraps it for a scroll
;;; box, which owns the offset.

(define-module (promptui widgets message-list)
  #:use-module (promptui width)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (promptui text)
  #:use-module (promptui theme)
  #:use-module (promptui padding)
  #:use-module (promptui widgets common)
  #:use-module (promptui widgets scroll-box)
  #:use-module (promptui widgets truncated-block)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-message-block
            message-block?
            message-block-role
            message-block-content
            message-block-set-content!
            message-block-border
            message-block-marker
            make-tool-call-block
            tool-call-block?
            tool-call-block-name
            tool-call-block-content
            make-questionnaire-summary-block
            questionnaire-summary-block?
            make-todo-list-block
            todo-list-block?
            todo-list-block-title
            todo-list-block-items
            todo-list-block-set-items!
            todo-list-block-check!
            make-message-list
            message-list?
            message-list-blocks
            message-list-length
            message-list-empty?
            message-list-ref
            message-list-push!
            message-list-push-message!
            message-list-clear!
            message-list-set-last-content!
            message-list-display-mode
            message-list-set-display-mode!
            message-list-set-block-override!
            message-list-clear-block-override!
            message-list-clear-overrides!
            message-list-set-padding!
            message-list-total-word-count
            message-list-height
            message-list-render!
            message-list-scrollable))

;;; Blocks

(define-record-type <message-block>
  (%make-message-block role content border marker marker-style style)
  message-block?
  ;; user, assistant or system.
  (role    message-block-role)
  ;; Anything ->text accepts.
  (content message-block-content message-block-set-content!)
  ;; A colour for the bar, overriding the one the role implies.
  (border  message-block-border)
  ;; A string drawn in place of the bar, or #f.
  (marker       message-block-marker)
  (marker-style message-block-marker-style)
  ;; The style of the text, or #f for the theme's.
  (style        message-block-style))

(define* (make-message-block role content #:key (border #f) (marker #f)
                             (marker-style #f) (style #f))
  (%make-message-block role content border marker marker-style style))

(define-record-type <tool-call-block>
  (make-tool-call-block name content)
  tool-call-block?
  (name    tool-call-block-name)
  ;; A truncated block.
  (content tool-call-block-content))

(define-record-type <questionnaire-summary-block>
  (make-questionnaire-summary-block text answered total)
  questionnaire-summary-block?
  (text     qs-text)
  (answered qs-answered)
  (total    qs-total))

(define-record-type <todo-list-block>
  (%make-todo-list-block title items)
  todo-list-block?
  (title todo-list-block-title)
  ;; A list of (LABEL . DONE?).
  (items todo-list-block-items todo-list-block-set-items!))

(define (make-todo-list-block title items)
  (%make-todo-list-block title items))

(define (todo-list-block-check! block index)
  "Mark item INDEX done."
  (todo-list-block-set-items!
   block
   (map (lambda (item i) (if (= i index) (cons (car item) #t) item))
        (todo-list-block-items block)
        (iota (length (todo-list-block-items block))))))

;;; The list

(define-record-type <message-list>
  (%make-message-list blocks display-mode overrides padding theme empty-hint)
  message-list?
  (blocks       message-list-blocks set-blocks!)
  ;; full or compact.
  (display-mode message-list-display-mode message-list-set-display-mode!)
  ;; An alist of block index to mode.
  (overrides    ml-overrides set-overrides!)
  (padding      ml-padding message-list-set-padding!)
  (theme        ml-theme)
  (empty-hint   ml-empty-hint))

(define* (make-message-list theme #:key
                            (padding (padding-uniform 0))
                            (display-mode 'full)
                            (empty-hint "Type a message and press Enter"))
  (%make-message-list '() display-mode '() padding theme empty-hint))

(define (message-list-length ml) (length (message-list-blocks ml)))
(define (message-list-empty? ml) (null? (message-list-blocks ml)))

(define (message-list-ref ml i)
  (let ((blocks (message-list-blocks ml)))
    (and (>= i 0) (< i (length blocks)) (list-ref blocks i))))

(define (message-list-push! ml block)
  "Append a block. Returns it."
  (set-blocks! ml (append (message-list-blocks ml) (list block)))
  block)

(define* (message-list-push-message! ml role content #:key (border #f) (marker #f)
                                     (marker-style #f) (style #f))
  (message-list-push! ml (make-message-block role content #:border border #:marker marker
                                             #:marker-style marker-style #:style style)))

(define (message-list-clear! ml)
  (set-blocks! ml '())
  (set-overrides! ml '()))

(define (message-list-set-last-content! ml content)
  "Replace the content of the last message block, which is how a streamed reply
grows in place."
  (let ((hit (find message-block? (reverse (message-list-blocks ml)))))
    (when hit (message-block-set-content! hit content))))

(define (message-list-set-block-override! ml index mode)
  (set-overrides! ml (cons (cons index mode)
                           (alist-delete index (ml-overrides ml) =))))

(define (message-list-clear-block-override! ml index)
  (set-overrides! ml (alist-delete index (ml-overrides ml) =)))

(define (message-list-clear-overrides! ml)
  (set-overrides! ml '()))

(define (effective-mode ml index)
  (or (assv-ref (ml-overrides ml) index) (message-list-display-mode ml)))

(define (message-list-total-word-count ml)
  "Words across every message, a stand-in for a token count."
  (apply + (map (lambda (b)
                  (if (message-block? b)
                      (length (filter (lambda (w) (not (string-null? w)))
                                      (string-tokenize (text-plain (message-block-content b)))))
                      0))
                (message-list-blocks ml))))

;;; Measuring

(define (h-overhead ml)
  (+ %border-width (padding-horizontal (ml-padding ml))))

(define (marker-extra block)
  "Columns a marker takes beyond the bar it replaces."
  (if (and (message-block? block) (message-block-marker block))
      (max 0 (- (string-width (message-block-marker block)) %border-width))
      0))

(define (block-height ml block text-width mode)
  (let ((pad-v (padding-vertical (ml-padding ml))))
    (cond
     ((message-block? block)
      (if (eq? mode 'compact)
          (+ 1 pad-v)
          (+ (text-height (message-block-content block)
                          (max 1 (- text-width (marker-extra block))))
             pad-v)))
     ((tool-call-block? block)
      (if (eq? mode 'compact)
          (+ 1 pad-v)
          ;; The name on its own line, then the output with its own bar.
          (+ 1 (truncated-block-required-height (tool-call-block-content block)
                                                (+ text-width %border-width)))))
     ((questionnaire-summary-block? block) (+ 1 pad-v))
     ((todo-list-block? block)
      (if (eq? mode 'compact)
          (+ 1 pad-v)
          (+ 1 (length (todo-list-block-items block)) pad-v)))
     (else (+ 1 pad-v)))))

(define (block-heights ml text-width)
  (map (lambda (b i) (block-height ml b text-width (effective-mode ml i)))
       (message-list-blocks ml)
       (iota (message-list-length ml))))

(define (total-height heights)
  ;; One blank row between consecutive blocks.
  (if (null? heights) 0 (+ (apply + heights) (- (length heights) 1))))

(define (message-list-height ml width)
  "Rows the whole list needs at WIDTH."
  (let ((text-width (max 0 (- width (h-overhead ml)))))
    (if (zero? text-width) 0 (total-height (block-heights ml text-width)))))

;;; Drawing

(define (role-color theme role)
  (case role
    ((user) (theme-primary theme))
    ((assistant) (theme-secondary theme))
    (else (theme-info theme))))

(define* (message-list-render! ml area buf #:optional (scroll 0))
  "Draw the list into AREA, bottom-anchored, SCROLL rows up from the newest."
  (unless (rect-empty? area)
    (let* ((theme (ml-theme ml))
           (pad (ml-padding ml))
           (text-x (+ (rect-x area) %border-width (padding-left pad)))
           (text-width (max 0 (- (rect-width area) (h-overhead ml)))))
      (fill-background! buf area (make-style #:bg (theme-panel-bg theme)))
      (cond
       ((message-list-empty? ml)
        (draw-text! buf text-x (+ (rect-y area) (quotient (rect-height area) 2))
                    (rect-right area) (ml-empty-hint ml) (theme-muted-style theme)))
       ((zero? text-width) #f)
       (else
        (let* ((heights (block-heights ml text-width))
               (total (total-height heights))
               (visible (rect-height area))
               (scroll (min scroll (max 0 (- total visible))))
               (content-bottom (max 0 (- total scroll)))
               (content-top (max 0 (- content-bottom visible))))
          (let loop ((blocks (message-list-blocks ml)) (hs heights) (i 0) (offset 0))
            (unless (null? blocks)
              (let* ((h (car hs))
                     (top offset)
                     (bottom (+ offset h)))
                (when (and (> bottom content-top) (< top content-bottom))
                  (let ((screen-top (+ (rect-y area) (max 0 (- top content-top))))
                        (screen-bottom (+ (rect-y area) (- (min bottom content-bottom)
                                                           content-top))))
                    (when (> screen-bottom screen-top)
                      (render-block ml (car blocks) (effective-mode ml i) area buf
                                    screen-top screen-bottom text-x text-width
                                    (max 0 (- content-top (+ top (padding-top pad))))))))
                (loop (cdr blocks) (cdr hs) (+ i 1) (+ bottom 1))))))))))
  buf)

(define (render-block ml block mode area buf top bottom text-x text-width skip)
  (let* ((theme (ml-theme ml))
         (pad (ml-padding ml))
         (bar (lambda (color) (draw-left-bar! buf area (theme-fg-style theme color) top bottom)))
         (first-row (+ top (padding-top pad)))
         (x-max (+ text-x text-width)))
    (cond
     ((message-block? block)
      (let* ((marker (message-block-marker block))
             (extra (marker-extra block))
             (text-x (+ text-x extra))
             (text-width (max 0 (- text-width extra)))
             (style (or (message-block-style block) (theme-text-style theme))))
        (if marker
            ;; Only on the block's first line: scrolled past it, the text continues
            ;; under a blank gutter.
            (when (and (zero? skip) (< first-row bottom))
              (draw-text! buf (rect-x area) first-row text-x marker
                          (or (message-block-marker-style block) style)))
            (bar (or (message-block-border block)
                     (role-color theme (message-block-role block)))))
        (if (eq? mode 'compact)
            (when (< first-row bottom)
              (let* ((plain (text-plain (message-block-content block)))
                     (first (text-first-line (message-block-content block)))
                     (more? (or (string-index plain #\newline)
                                (> (string-width first) text-width)))
                     (room (if (and more? (> text-width 3)) (- text-width 3) text-width))
                     (x (draw-text! buf text-x first-row x-max
                                    (string-take-width first room)
                                    style)))
                (when more?
                  (draw-text! buf x first-row (rect-right area) "..."
                              (theme-muted-style theme)))))
            (let ((text-area (make-rect text-x first-row text-width
                                        (max 0 (- bottom top (padding-vertical pad))))))
              (unless (rect-empty? text-area)
                (render-paragraph! buf text-area (message-block-content block)
                                   #:style style
                                   #:scroll skip))))))

     ((tool-call-block? block)
      (let ((label (string-append "Tool: " (tool-call-block-name block))))
        (if (eq? mode 'compact)
            (begin
              (bar (theme-border theme))
              (when (< first-row bottom)
                (draw-text! buf text-x first-row x-max label (theme-muted-style theme))))
            (begin
              (draw-text! buf text-x top x-max label (theme-muted-style theme))
              (when (< (+ top 1) bottom)
                ;; The output carries its own bar, one column left of the text.
                (truncated-block-render! (tool-call-block-content block)
                                         (make-rect (- text-x %border-width) (+ top 1)
                                                    (+ text-width %border-width)
                                                    (- bottom top 1))
                                         buf))))))

     ((questionnaire-summary-block? block)
      (bar (theme-accent theme))
      (when (< first-row bottom)
        (draw-text! buf text-x first-row x-max
                    (format #f "Survey: ~a · ~a/~a"
                            (qs-text block) (qs-answered block) (qs-total block))
                    (theme-muted-style theme))))

     ((todo-list-block? block)
      (bar (theme-secondary theme))
      (let* ((items (todo-list-block-items block))
             (done (count cdr items))
             (title (format #f "~a (~a/~a)" (todo-list-block-title block)
                            done (length items))))
        (if (eq? mode 'compact)
            (when (< first-row bottom)
              (draw-text! buf text-x first-row x-max title (theme-muted-style theme)))
            (when (< first-row bottom)
              (draw-text! buf text-x first-row x-max title (theme-text-style theme))
              (let loop ((items items) (y (+ first-row 1)))
                (when (and (pair? items) (< y bottom))
                  (let* ((done? (cdar items))
                         (x (draw-text! buf text-x y x-max
                                        (if done? "[✓]" "[ ]")
                                        (if done?
                                            (theme-fg-style theme (theme-success theme))
                                            (theme-muted-style theme)))))
                    (draw-text! buf (+ x 1) y x-max (caar items)
                                (if done? (theme-muted-style theme) (theme-text-style theme)))
                    (loop (cdr items) (+ y 1)))))))))
     (else #f))))

(define (message-list-scrollable ml)
  "ML as something a scroll box can scroll."
  (make-scrollable (lambda (width) (message-list-height ml width))
                   (lambda (area buf offset) (message-list-render! ml area buf offset))))
