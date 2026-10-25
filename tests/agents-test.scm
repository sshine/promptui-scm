(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (harness)
             (promptui style)
             (promptui rect)
             (promptui buffer)
             (promptui theme)
             (promptui padding)
             (promptui focus)
             (promptui widgets agent-scope)
             (promptui widgets agent-overview)
             (promptui widgets questionnaire))

(define theme %opencode-theme)

(define (header label . opts)
  (apply make-agent-scope-header theme label opts))

(define (overview n)
  (let ((ao (make-agent-overview theme)))
    (for-each (lambda (i)
                (agent-overview-push! ao (make-agent-entry i (format #f "agent-~a" i)
                                                           #:input-tokens (* 1000 (+ i 1))
                                                           #:output-tokens 100)))
              (iota n))
    ao))

(define (sample-questions)
  (list (make-question 'single "Which approach?"
                       (list (make-question-option "Option A")
                             (make-question-option "Option B")
                             (make-question-option "Other" #:inline-text? #t)))
        (make-question 'multi "Which features?"
                       (list (make-question-option "Logging")
                             (make-question-option "Metrics")
                             (make-question-option "Tracing")))
        (make-question 'free-text "Additional notes?")))

(define (focused-questionnaire)
  (let ((qn (make-questionnaire theme (sample-questions))))
    (questionnaire-on-focus! qn)
    qn))

(define (option-selected? qn q o)
  (question-option-selected?
   (list-ref (question-options (list-ref (questionnaire-questions qn) q)) o)))

(run-suite
 "agents"
 (lambda ()

   (test-group "focus stack"
     (let ((stack (make-focus-stack)))
       (test-equal 'text-field (focus-top stack))
       (test-equal 1 (focus-depth stack))
       (test-assert (focus-push! stack 'agent-overview))
       (test-assert (not (focus-push! stack 'agent-overview)))
       (test-assert (focus-push! stack 'questionnaire))
       (test-assert (focus-contains? stack 'text-field))
       (test-equal 'questionnaire (focus-pop! stack))
       (test-equal 'agent-overview (focus-pop! stack))
       ;; The base is never popped.
       (test-equal #f (focus-pop! stack))
       (test-equal 'text-field (focus-top stack))))

   (test-group "agent formatting"
     (test-equal "●" (agent-status-icon 'active))
     (test-equal "✓" (agent-status-icon 'done))
     (test-equal "✗" (agent-status-icon 'error))
     (test-equal "—" (agent-status-icon 'cancelled))
     (test-equal "12s" (format-duration 12))
     (test-equal "2m" (format-duration 150))
     (test-equal "1h" (format-duration 3700))
     (test-equal "999" (format-tokens 999))
     (test-equal "2.1k" (format-tokens 2100))
     (test-equal "15k" (format-tokens 14600))
     (test-equal "1.2M" (format-tokens 1200000)))

   (test-group "agent scope header"
     (test-equal 1 (agent-scope-header-height (header "main")))
     (test-equal 3 (agent-scope-header-height (header "main" #:padding (padding-uniform 1))))
     (test-equal "● research" (agent-scope-header-text (header "research")))
     (test-equal "● research (12s · 2.1k)"
       (agent-scope-header-text (header "research" #:duration 12 #:token-count 2100)))
     (test-equal "✓ done" (agent-scope-header-text (header "done" #:status 'done)))
     (test-equal '("━━ ● main ━━━━━━━━━━")
       (render-lines 20 1 (lambda (area buf)
                            (agent-scope-header-render! (header "main") area buf))))
     (test-equal "├─ ● child"
       (string-trim-right
        (car (render-lines 20 1 (lambda (area buf)
                                  (agent-scope-header-render! (header "child" #:depth 1)
                                                              area buf))))))
     (test-equal "│ └─ ● leaf"
       (string-trim-right
        (car (render-lines 20 1 (lambda (area buf)
                                  (agent-scope-header-render!
                                   (header "leaf" #:depth 2 #:last-sibling? #t)
                                   area buf))))))
     (test-equal '()
       (render-lines 0 0 (lambda (area buf)
                           (agent-scope-header-render! (header "x") area buf)))))

   (test-group "agent overview heights"
     (test-equal 0 (agent-overview-required-height (overview 0)))
     (test-equal 0 (agent-overview-required-height (overview 1)))
     (let ((ao (overview 5)))
       (test-equal 1 (agent-overview-required-height ao))
       (test-equal 'summary (agent-overview-cycle-mode! ao))
       ;; Rounded frame plus three rows, the most a summary shows.
       (test-equal 5 (agent-overview-required-height ao))
       (test-equal 'full (agent-overview-cycle-mode! ao))
       (test-equal 0 (agent-overview-required-height ao))
       (test-equal 'collapsed (agent-overview-cycle-mode! ao))))

   (test-group "agent overview navigation and checking"
     (let ((ao (overview 3)))
       (agent-overview-focus-up! ao)
       (test-equal 0 (agent-overview-focused ao))
       (agent-overview-focus-down! ao)
       (agent-overview-focus-down! ao)
       (agent-overview-focus-down! ao)
       (test-equal 2 (agent-overview-focused ao))
       (test-equal 3 (agent-overview-checked-count ao))
       (agent-overview-toggle-focused! ao)
       (test-equal '(0 1) (agent-overview-checked-ids ao))
       (test-equal 6000 (agent-overview-total-input-tokens ao))
       (test-equal 300 (agent-overview-total-output-tokens ao))))

   (test-group "agent overview input in full mode"
     (let ((ao (overview 3)))
       (agent-overview-cycle-mode! ao)
       (agent-overview-cycle-mode! ao)
       (agent-overview-on-focus! ao)
       (test-equal 'consumed (agent-overview-handle-input! ao 'down))
       (test-equal 'consumed (agent-overview-handle-input! ao 'enter))
       (test-equal 1 (agent-overview-viewing ao))
       (test-equal 'consumed (agent-overview-handle-input! ao 'esc))
       (test-equal #f (agent-overview-viewing ao))
       (test-equal 'pop (agent-overview-handle-input! ao 'esc))
       (test-equal 'summary (agent-overview-mode ao))))

   (test-group "agent overview drawing"
     (let ((ao (overview 2)))
       (agent-overview-push-message! ao 0 'assistant "hello from zero")
       (let ((row (car (render-lines 40 1 (lambda (area buf)
                                            (agent-overview-render! ao area buf))))))
         (test-assert (string-prefix? "●●  2 agents  ↑3.0k ↓200  Ctrl+A" row)))
       (agent-overview-cycle-mode! ao)
       (let ((lines (render-lines 40 4 (lambda (area buf)
                                         (agent-overview-render! ao area buf)))))
         (test-assert (string-prefix? "╭─ Agents (2) ─" (car lines)))
         (test-assert (string-contains (cadr lines) "[×] ● agent-0")))
       (agent-overview-cycle-mode! ao)
       (let ((lines (render-lines 40 2 (lambda (area buf)
                                         (agent-overview-render-full! ao area buf)))))
         (test-assert (string-prefix? "● agent-0" (car lines)))
         (test-assert (string-contains (car lines) "↑1.0k ↓100")))
       (agent-overview-handle-input! ao 'enter)
       (let ((lines (render-lines 40 5 (lambda (area buf)
                                         (agent-overview-render-full! ao area buf)))))
         (test-equal "Agents › agent-0" (string-trim-right (car lines)))
         (test-assert (any (lambda (l) (string-contains l "hello from zero")) lines)))))

   (test-group "a new questionnaire"
     (let ((qn (make-questionnaire theme (sample-questions))))
       (test-equal 3 (questionnaire-count qn))
       (test-equal 0 (questionnaire-answered-count qn))
       (test-assert (not (questionnaire-completed? qn)))
       (test-assert (not (questionnaire-cancelled? qn)))
       (test-equal "Survey: 0/3 answered" (questionnaire-summary-text qn))))

   (test-group "enter selects and advances"
     (let ((qn (focused-questionnaire)))
       (questionnaire-handle-input! qn 'enter)
       (test-assert (option-selected? qn 0 0))
       (test-equal 1 (questionnaire-current-tab qn))))

   (test-group "space selects without advancing"
     (let ((qn (focused-questionnaire)))
       (questionnaire-handle-input! qn #\space)
       (test-assert (option-selected? qn 0 0))
       (test-equal 0 (questionnaire-current-tab qn))))

   (test-group "backspace clears the question"
     (let ((qn (focused-questionnaire)))
       (questionnaire-handle-input! qn 'down)
       (questionnaire-handle-input! qn #\space)
       (test-assert (option-selected? qn 0 1))
       (questionnaire-handle-input! qn 'backspace)
       (test-assert (not (question-answered? (car (questionnaire-questions qn)))))))

   (test-group "an option with its own text is edited in place"
     (let ((qn (focused-questionnaire)))
       (questionnaire-handle-input! qn 'down)
       (questionnaire-handle-input! qn 'down)
       (test-equal 'inline-text (questionnaire-page qn))
       (for-each (lambda (ch) (questionnaire-handle-input! qn ch)) (string->list "mine"))
       (test-assert (questionnaire-cursor-position qn (make-rect 0 0 60 12)))
       (questionnaire-handle-input! qn 'enter)
       (test-equal "Other: mine"
         (question-answer-summary (car (questionnaire-questions qn))))))

   (test-group "the last question leads to the summary, which commits"
     (let ((qn (focused-questionnaire)))
       (questionnaire-handle-input! qn 'right)
       (questionnaire-handle-input! qn 'right)
       ;; The free-text question opens straight into its field.
       (test-equal 'free-text (questionnaire-page qn))
       (for-each (lambda (ch) (questionnaire-handle-input! qn ch)) (string->list "none"))
       (questionnaire-handle-input! qn 'enter)
       (test-equal 'summary (questionnaire-page qn))
       (test-equal "none" (question-text (caddr (questionnaire-questions qn))))
       (test-equal 0 (questionnaire-summary-action qn))
       (questionnaire-handle-input! qn 'tab)
       (test-equal 1 (questionnaire-summary-action qn))
       (questionnaire-handle-input! qn 'tab)
       (test-equal 0 (questionnaire-summary-action qn))
       (let ((lines (render-lines 60 10 (lambda (area buf)
                                          (questionnaire-render! qn area buf)))))
         (test-equal " Summary" (string-trim-right (car lines)))
         (test-assert (any (lambda (l) (string-contains l "(unanswered)")) lines))
         (test-assert (any (lambda (l) (string-contains l "[Accept]")) lines)))
       (test-equal 'pop (questionnaire-handle-input! qn 'enter))
       (test-assert (questionnaire-completed? qn))
       (test-assert (not (questionnaire-cancelled? qn)))))

   (test-group "escape from the summary goes back to the last question"
     (let ((qn (focused-questionnaire)))
       (questionnaire-handle-input! qn 'right)
       (questionnaire-handle-input! qn 'right)
       (questionnaire-handle-input! qn 'enter)
       (test-equal 'consumed (questionnaire-handle-input! qn 'esc))
       (test-equal 2 (questionnaire-current-tab qn))
       (test-assert (not (eq? 'summary (questionnaire-page qn))))))

   (test-group "escape from the options cancels"
     (let ((qn (focused-questionnaire)))
       (test-equal 'pop (questionnaire-handle-input! qn 'esc))
       (test-assert (questionnaire-cancelled? qn))
       (test-assert (not (questionnaire-completed? qn)))))

   (test-group "questionnaire drawing"
     (let ((qn (focused-questionnaire)))
       (let ((lines (map string-trim-right
                         (render-lines 80 8 (lambda (area buf)
                                              (questionnaire-render! qn area buf))))))
         (test-equal " ● ○ ○" (car lines))
         (test-equal " Which approach?" (cadr lines))
         (test-equal " › ○ Option A" (list-ref lines 3))
         (test-equal "   ○ Other: " (string-append (list-ref lines 5) " ")))
       (questionnaire-on-blur! qn)
       (test-equal "┃Survey: 0/3 answered"
         (string-trim-right
          (car (render-lines 40 1 (lambda (area buf)
                                    (questionnaire-render! qn area buf))))))))))
