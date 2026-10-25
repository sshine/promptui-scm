;;; A multi-question form with one tab per question and a summary at the end.
;;;
;;; A question is single-select, multi-select or free text, and an option can carry
;;; a line of text of its own ("Other: ..."). Enter selects and moves to the next
;;; tab; after the last one comes a summary where Tab chooses between running the
;;; plan with permission per action (Accept) or without (Auto), and Enter commits.
;;; Escape from the option list cancels.
;;;
;;; Unfocused, the questionnaire collapses to a single summary line.

(define-module (promptui widgets questionnaire)
  #:use-module (promptui width)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (promptui theme)
  #:use-module (promptui padding)
  #:use-module (promptui widgets common)
  #:use-module (promptui widgets text-field)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-question-option
            question-option?
            question-option-label
            question-option-selected?
            question-option-inline-text
            make-question
            question?
            question-prompt
            question-kind
            question-options
            question-text
            question-answered?
            question-answer-summary
            make-questionnaire
            questionnaire?
            questionnaire-questions
            questionnaire-current-question
            questionnaire-current-tab
            questionnaire-page
            questionnaire-count
            questionnaire-answered-count
            questionnaire-focused?
            questionnaire-completed?
            questionnaire-cancelled?
            questionnaire-summary-action
            questionnaire-summary-text
            questionnaire-set-padding!
            questionnaire-required-height
            questionnaire-cursor-position
            questionnaire-handle-input!
            questionnaire-on-focus!
            questionnaire-on-blur!
            questionnaire-render!))

(define %dot-filled "●")       ; ●
(define %dot-empty "○")        ; ○
(define %check-selected "[×]") ; [×]
(define %check-unselected "[ ]")
(define %option-cursor "›")    ; ›
(define %edit-cursor "█")      ; █

;;; Options and questions

(define-record-type <question-option>
  (%make-option label selected inline-text)
  question-option?
  (label       question-option-label)
  (selected    question-option-selected? set-option-selected!)
  ;; #f, or the option's own line of text.
  (inline-text question-option-inline-text set-option-inline-text!))

(define* (make-question-option label #:key (selected #f) (inline-text? #f))
  (%make-option label selected (and inline-text? "")))

(define-record-type <question>
  (%make-question prompt kind options text)
  question?
  (prompt  question-prompt)
  ;; single, multi or free-text.
  (kind    question-kind)
  (options question-options)
  (text    question-text set-question-text!))

(define* (make-question kind prompt #:optional (options '()))
  (%make-question prompt kind options ""))

(define (question-answered? q)
  (if (eq? (question-kind q) 'free-text)
      (not (string-null? (string-trim-both (question-text q))))
      (any question-option-selected? (question-options q))))

(define (question-answer-summary q)
  (case (question-kind q)
    ((single)
     (let ((o (find question-option-selected? (question-options q))))
       (cond
        ((not o) "")
        ((and (question-option-inline-text o)
              (not (string-null? (question-option-inline-text o))))
         (string-append (question-option-label o) ": " (question-option-inline-text o)))
        (else (question-option-label o)))))
    ((multi)
     (string-join (map question-option-label
                       (filter question-option-selected? (question-options q)))
                  ", "))
    (else (question-text q))))

;;; The questionnaire

(define-record-type <questionnaire>
  (%make-questionnaire theme padding questions tab option inner focused field action
                       completed cancelled)
  questionnaire?
  (theme     qn-theme)
  (padding   qn-padding questionnaire-set-padding!)
  (questions questionnaire-questions)
  (tab       questionnaire-current-tab set-tab!)
  (option    qn-option set-option!)
  ;; options, inline-text, free-text or summary.
  (inner     questionnaire-page set-inner!)
  (focused   questionnaire-focused? set-focused!)
  (field     qn-field)
  ;; On the summary page: 0 is Accept, 1 is Auto.
  (action    questionnaire-summary-action set-action!)
  (completed questionnaire-completed? set-completed!)
  (cancelled questionnaire-cancelled? set-cancelled!))

(define* (make-questionnaire theme questions #:key (padding (padding-uniform 0)))
  (%make-questionnaire theme padding questions 0 0 'options #f
                       (make-text-field theme #:padding (padding-uniform 0)
                                        #:min-height 1 #:max-height 1 #:focused #f)
                       0 #f #f))

(define (questionnaire-count qn) (length (questionnaire-questions qn)))

(define (questionnaire-current-question qn)
  (let ((qs (questionnaire-questions qn)))
    (and (< (questionnaire-current-tab qn) (length qs))
         (list-ref qs (questionnaire-current-tab qn)))))

(define (questionnaire-answered-count qn)
  (count question-answered? (questionnaire-questions qn)))

(define (questionnaire-summary-text qn)
  (let ((answer (let ((q (questionnaire-current-question qn)))
                  (if q (question-answer-summary q) "")))
        (progress (format #f "~a/~a answered"
                          (questionnaire-answered-count qn) (questionnaire-count qn))))
    (if (string-null? answer)
        (string-append "Survey: " progress)
        (format #f "Survey: ~a · ~a" answer progress))))

(define (questionnaire-required-height qn width)
  (let ((pad (padding-vertical (qn-padding qn)))
        (q (questionnaire-current-question qn)))
    (cond
     ;; Header, gap, one row per question, gap, the mode toggle and the hints.
     ((eq? (questionnaire-page qn) 'summary) (+ (questionnaire-count qn) 5 pad))
     ((not q) 0)
     ((eq? (question-kind q) 'free-text) (+ 8 pad))
     (else (+ (length (question-options q)) 5 pad)))))

(define (indicator-width kind)
  (case kind ((single) 2) ((multi) 4) (else 0)))

(define (questionnaire-cursor-position qn area)
  "Where the terminal cursor belongs while a line of text is being edited, or #f."
  (and (questionnaire-focused? qn)
       (let ((pad (qn-padding qn))
             (q (questionnaire-current-question qn))
             (typed (string-width (text-field-content (qn-field qn)))))
         (let ((text-x (+ (rect-x area) %border-width (padding-left pad)))
               (top (+ (rect-y area) (padding-top pad))))
           (case (questionnaire-page qn)
             ((inline-text)
              (let* ((y (+ top 3 (qn-option qn)))
                     (o (list-ref (question-options q) (qn-option qn)))
                     (x (+ text-x 2 (indicator-width (question-kind q))
                           (string-width (question-option-label o)) 2 typed)))
                (and (< y (rect-bottom area)) (< x (rect-right area)) (cons x y))))
             ((free-text)
              (let ((x (+ text-x typed)) (y (+ top 3)))
                (and (< y (rect-bottom area)) (< x (rect-right area)) (cons x y))))
             (else #f))))))

;;; Navigation

(define (current-option qn)
  (let ((q (questionnaire-current-question qn)))
    (and q (< (qn-option qn) (length (question-options q)))
         (list-ref (question-options q) (qn-option qn)))))

(define (save-inline-text! qn)
  (let ((content (text-field-content (qn-field qn))))
    (case (questionnaire-page qn)
      ((inline-text)
       (let ((o (current-option qn)))
         (when (and o (question-option-inline-text o))
           (set-option-inline-text! o content)))
       (text-field-set-focused! (qn-field qn) #f))
      ((free-text)
       (set-question-text! (questionnaire-current-question qn) content)
       (text-field-set-focused! (qn-field qn) #f))
      (else #f))))

(define (try-activate-inline! qn)
  "Start editing when the focused option has a line of text, or the question is free
text; otherwise go back to choosing among the options."
  (let* ((q (questionnaire-current-question qn))
         (o (current-option qn))
         (mode+text (cond
                     ((not q) #f)
                     ((eq? (question-kind q) 'free-text) (cons 'free-text (question-text q)))
                     ((and o (question-option-inline-text o))
                      (cons 'inline-text (question-option-inline-text o)))
                     (else #f)))
         (field (qn-field qn)))
    (if mode+text
        (begin
          (text-field-set-content! field (cdr mode+text))
          (text-field-set-focused! field #t)
          (set-inner! qn (car mode+text)))
        (begin
          (text-field-set-focused! field #f)
          (set-inner! qn 'options)))))

(define (move-option! qn delta)
  (let* ((q (questionnaire-current-question qn))
         (n (if q (length (question-options q)) 0))
         (target (+ (qn-option qn) delta)))
    (when (and (>= target 0) (< target n))
      (save-inline-text! qn)
      (set-option! qn target)
      (try-activate-inline! qn))))

(define (move-tab! qn delta)
  (let ((target (+ (questionnaire-current-tab qn) delta)))
    (when (and (>= target 0) (< target (questionnaire-count qn)))
      (save-inline-text! qn)
      (set-tab! qn target)
      (set-option! qn 0)
      (set-inner! qn 'options)
      (try-activate-inline! qn))))

(define (select-current! qn)
  (let ((q (questionnaire-current-question qn)))
    (when q
      (case (question-kind q)
        ((single)
         (for-each (lambda (o i) (set-option-selected! o (= i (qn-option qn))))
                   (question-options q) (iota (length (question-options q)))))
        ((multi)
         (let ((o (current-option qn)))
           (when o (set-option-selected! o (not (question-option-selected? o))))))
        (else #f)))))

(define (select-and-advance! qn)
  (select-current! qn)
  (if (< (+ 1 (questionnaire-current-tab qn)) (questionnaire-count qn))
      (move-tab! qn 1)
      (begin (save-inline-text! qn) (set-inner! qn 'summary))))

(define (clear-current-question! qn)
  (let ((q (questionnaire-current-question qn)))
    (when q
      (for-each (lambda (o)
                  (set-option-selected! o #f)
                  (when (question-option-inline-text o) (set-option-inline-text! o "")))
                (question-options q))
      (set-question-text! q ""))))

(define (leave-field! qn)
  (save-inline-text! qn)
  (set-inner! qn 'options)
  (text-field-set-focused! (qn-field qn) #f))

(define (questionnaire-handle-input! qn input)
  "INPUT is a character or an action symbol from key->input. Returns consumed, pop
or ignored."
  (case (questionnaire-page qn)
    ((inline-text free-text)
     (let ((free? (eq? (questionnaire-page qn) 'free-text)))
       (case input
         ((esc) (leave-field! qn) 'consumed)
         ((up) (leave-field! qn) (unless free? (move-option! qn -1)) 'consumed)
         ((down) (leave-field! qn) (unless free? (move-option! qn 1)) 'consumed)
         ((enter) (leave-field! qn) (select-and-advance! qn) 'consumed)
         (else (text-field-input! (qn-field qn) input) 'consumed))))
    ((options)
     (cond
      ((eq? input 'esc) (set-cancelled! qn #t) 'pop)
      ((eq? input 'up) (move-option! qn -1) 'consumed)
      ((eq? input 'down) (move-option! qn 1) 'consumed)
      ((eq? input 'left) (move-tab! qn -1) 'consumed)
      ((eq? input 'right) (move-tab! qn 1) 'consumed)
      ((eq? input 'enter) (select-and-advance! qn) 'consumed)
      ((eqv? input #\space) (select-current! qn) 'consumed)
      ((eq? input 'backspace) (clear-current-question! qn) 'consumed)
      (else 'ignored)))
    ((summary)
     (case input
       ((tab) (set-action! qn (modulo (+ 1 (questionnaire-summary-action qn)) 2)) 'consumed)
       ((enter) (set-completed! qn #t) 'pop)
       ((esc)
        (set-tab! qn (max 0 (- (questionnaire-count qn) 1)))
        (set-option! qn 0)
        (set-inner! qn 'options)
        (try-activate-inline! qn)
        'consumed)
       (else 'ignored)))
    (else 'ignored)))

(define (questionnaire-on-focus! qn)
  (set-focused! qn #t)
  (set-completed! qn #f)
  (set-cancelled! qn #f)
  (try-activate-inline! qn))

(define (questionnaire-on-blur! qn)
  (set-focused! qn #f)
  (save-inline-text! qn)
  (set-inner! qn 'options)
  (text-field-set-focused! (qn-field qn) #f))

(define (hint-text qn)
  (case (questionnaire-page qn)
    ((options)
     "←/→ tabs  ↑/↓ options  Enter select+next  Backspace clear  Esc cancel")
    ((summary) "Tab toggle mode  Enter execute  Esc back")
    (else "Type to edit  Esc/↑/↓ exit field  ←/→ tabs")))

;;; Drawing

(define (questionnaire-render! qn area buf)
  (unless (rect-empty? area)
    (cond
     ((not (questionnaire-focused? qn)) (render-compact qn area buf))
     ((eq? (questionnaire-page qn) 'summary) (render-summary qn area buf))
     (else (render-full qn area buf))))
  buf)

(define (frame qn area)
  "The text column's start and end, the first row, and the row bound."
  (let ((pad (qn-padding qn)))
    (values (+ (rect-x area) %border-width (padding-left pad))
            (- (rect-right area) (padding-right pad))
            (+ (rect-y area) (padding-top pad))
            (- (rect-bottom area) (padding-bottom pad)))))

(define (render-full qn area buf)
  (let* ((theme (qn-theme qn))
         (text (theme-text-style theme))
         (muted (theme-muted-style theme))
         (primary (theme-fg-style theme (theme-primary theme)))
         (q (questionnaire-current-question qn)))
    (fill-background! buf area (make-style #:bg (theme-panel-bg theme)))
    (call-with-values (lambda () (frame qn area))
      (lambda (text-x x-max top y-max)
        (when (and q (> x-max text-x))
          (let ((y top))
            ;; Tab dots: the current one and the answered ones filled.
            (when (< y y-max)
              (fold (lambda (question i x)
                      (let* ((current? (= i (questionnaire-current-tab qn)))
                             (answered? (question-answered? question))
                             (x (draw-text! buf x y x-max
                                            (if (or current? answered?) %dot-filled %dot-empty)
                                            (cond (current? primary)
                                                  (answered? (theme-fg-style theme (theme-success theme)))
                                                  (else muted)))))
                        (min x-max (+ x 1))))
                    text-x
                    (questionnaire-questions qn)
                    (iota (questionnaire-count qn)))
              (set! y (+ y 1)))
            (when (< y y-max)
              (draw-text! buf text-x y x-max (question-prompt q)
                          (style-add-modifier text 'bold))
              (set! y (+ y 1)))
            (when (< y y-max) (set! y (+ y 1)))
            (if (eq? (question-kind q) 'free-text)
                (when (< y y-max)
                  (let* ((editing? (eq? (questionnaire-page qn) 'free-text))
                         (x (draw-text! buf text-x y x-max
                                        (if editing?
                                            (text-field-content (qn-field qn))
                                            (question-text q))
                                        (if editing? text muted))))
                    (when (and editing? (< x x-max))
                      (draw-text! buf x y x-max %edit-cursor primary))
                    (set! y (+ y 1))))
                (for-each
                 (lambda (o i)
                   (when (< y y-max)
                     (let* ((focused? (= i (qn-option qn)))
                            (x (if focused?
                                   (draw-text! buf text-x y x-max
                                               (string-append %option-cursor " ") primary)
                                   (+ text-x 2)))
                            (x (draw-text! buf x y x-max
                                           (if (eq? (question-kind q) 'single)
                                               (if (question-option-selected? o) %dot-filled %dot-empty)
                                               (if (question-option-selected? o)
                                                   %check-selected %check-unselected))
                                           (if (question-option-selected? o) primary muted)))
                            (x (min x-max (+ x 1)))
                            (x (draw-text! buf x y x-max (question-option-label o)
                                           (if (or focused? (question-option-selected? o))
                                               text muted))))
                       (when (question-option-inline-text o)
                         (let* ((editing? (and focused? (eq? (questionnaire-page qn) 'inline-text)))
                                (x (draw-text! buf x y x-max ": " muted))
                                (x (draw-text! buf x y x-max
                                               (if editing?
                                                   (text-field-content (qn-field qn))
                                                   (question-option-inline-text o))
                                               (if editing? text muted))))
                           (when (and editing? (< x x-max))
                             (draw-text! buf x y x-max %edit-cursor primary))))
                       (set! y (+ y 1)))))
                 (question-options q)
                 (iota (length (question-options q)))))
            (when (< y y-max) (set! y (+ y 1)))
            (when (< y y-max)
              (draw-text! buf text-x y x-max (hint-text qn) muted))))))))

(define (render-summary qn area buf)
  (let* ((theme (qn-theme qn))
         (text (theme-text-style theme))
         (muted (theme-muted-style theme))
         (primary (theme-fg-style theme (theme-primary theme))))
    (fill-background! buf area (make-style #:bg (theme-panel-bg theme)))
    (call-with-values (lambda () (frame qn area))
      (lambda (text-x x-max top y-max)
        (when (> x-max text-x)
          (let ((y top)
                (text-width (- x-max text-x)))
            (when (< y y-max)
              (draw-text! buf text-x y x-max "Summary" (style-add-modifier text 'bold))
              (set! y (+ y 1)))
            (when (< y y-max) (set! y (+ y 1)))
            (for-each
             (lambda (q)
               (when (< y y-max)
                 (let* ((answered? (question-answered? q))
                        (x (draw-text! buf text-x y x-max (if answered? "✓ " "  ")
                                       (if answered?
                                           (theme-fg-style theme (theme-success theme))
                                           muted)))
                        (room (min 20 (quotient text-width 2)))
                        (prompt (question-prompt q))
                        (x (draw-text! buf x y x-max
                                       (if (> (string-width prompt) room)
                                           (string-append
                                            (string-take-width prompt (max 0 (- room 3)))
                                            "...")
                                           prompt)
                                       text))
                        (x (draw-text! buf x y x-max " → " muted))
                        (answer (question-answer-summary q)))
                   (draw-text! buf x y x-max
                               (if (string-null? answer) "(unanswered)" answer)
                               (if answered? primary muted))
                   (set! y (+ y 1)))))
             (questionnaire-questions qn))
            (when (< y y-max) (set! y (+ y 1)))
            (when (< y y-max)
              (fold (lambda (mode i x)
                      (let ((selected? (= i (questionnaire-summary-action qn))))
                        (draw-text! buf x y x-max
                                    (if selected?
                                        (format #f " [~a] " mode)
                                        (format #f "  ~a  " mode))
                                    (if selected?
                                        (make-style #:fg (theme-panel-bg theme)
                                                    #:bg (theme-primary theme)
                                                    #:modifiers '(bold))
                                        muted))))
                    text-x '("Accept" "Auto") '(0 1))
              (set! y (+ y 1)))
            (when (< y y-max)
              (draw-text! buf text-x y x-max (hint-text qn) muted))))))))

(define (render-compact qn area buf)
  (let ((theme (qn-theme qn))
        (row (rect-row area 0)))
    (fill-background! buf row (make-style #:bg (theme-panel-bg theme)))
    (draw-left-bar! buf row (theme-fg-style theme (theme-secondary theme)))
    (call-with-values (lambda () (frame qn area))
      (lambda (text-x x-max top y-max)
        (draw-text! buf text-x (rect-y area) x-max (questionnaire-summary-text qn)
                    (theme-muted-style theme))))))
