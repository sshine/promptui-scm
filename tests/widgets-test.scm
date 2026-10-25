(use-modules (srfi srfi-1)
             (srfi srfi-64)
             (harness)
             (promptui style)
             (promptui rect)
             (promptui buffer)
             (promptui text)
             (promptui theme)
             (promptui padding)
             (promptui input)
             (promptui widgets scrollbar)
             (promptui widgets scroll-box)
             (promptui widgets spinner)
             (promptui widgets message-list)
             (promptui widgets truncated-block)
             (promptui widgets status-bar)
             (promptui widgets streaming-text)
             (promptui widgets border-box)
             (promptui widgets notice)
             (promptui widgets breadcrumb)
             (promptui widgets todo-list))

(define theme %opencode-theme)

(define (row-of lines n) (list-ref lines n))

(run-suite
 "widgets"
 (lambda ()

   (test-group "scrollbar"
     (test-assert (not (scrollbar-visible? (make-scrollbar 5 10 0))))
     (test-assert (not (scrollbar-visible? (make-scrollbar 10 10 0))))
     (test-assert (scrollbar-visible? (make-scrollbar 20 10 0)))
     ;; At position zero the thumb starts at the top.
     (let ((lines (render-lines 1 10 (lambda (area buf)
                                       (scrollbar-render! (make-scrollbar 20 10 0) area buf)))))
       (test-equal "█" (car lines))
       (test-equal " " (last lines))))

   (test-group "scroll box"
     (let* ((sb (make-scroll-box))
            (content (make-scrollable (const 30) (lambda (area buf offset) #f))))
       (scroll-box-render! sb (make-rect 0 0 10 10) (make-buffer (make-rect 0 0 10 10)) content)
       (scroll-box-scroll-up! sb 100)
       (test-equal 20 (scroll-box-offset sb))
       (scroll-box-scroll-down! sb 100)
       (test-equal 0 (scroll-box-offset sb))
       (scroll-box-page-up! sb)
       (test-equal 5 (scroll-box-offset sb))
       (scroll-box-page-down! sb)
       (test-equal 0 (scroll-box-offset sb))
       (scroll-box-scroll-up! sb 7)
       (scroll-box-scroll-to-bottom! sb)
       (test-assert (scroll-box-at-bottom? sb))))

   (test-group "scroll box and the mouse"
     (let* ((sb (make-scroll-box))
            (content (make-scrollable (const 30) (lambda (area buf offset) #f)))
            (draw! (lambda ()
                     (scroll-box-render! sb (make-rect 0 0 10 10)
                                         (make-buffer (make-rect 0 0 10 10)) content))))
       (draw!)
       (test-assert (scroll-box-handle-mouse! sb (make-mouse 'scroll-up 3 3)))
       (test-equal 3 (scroll-box-offset sb))
       (scroll-box-handle-mouse! sb (make-mouse 'scroll-down 3 3))
       (test-equal 0 (scroll-box-offset sb))
       ;; The wheel outside the box is someone else's.
       (test-assert (not (scroll-box-handle-mouse! sb (make-mouse 'scroll-up 3 12))))
       (test-equal 0 (scroll-box-offset sb))
       ;; A press at the top of the bar shows the oldest content.
       (test-assert (scroll-box-handle-mouse! sb (make-mouse 'press 9 0 #:button 'left)))
       (test-equal 20 (scroll-box-offset sb))
       ;; The drag holds on even off the bar, and ends at the bottom.
       (test-assert (scroll-box-handle-mouse! sb (make-mouse 'drag 2 9 #:button 'left)))
       (test-equal 0 (scroll-box-offset sb))
       (test-assert (scroll-box-handle-mouse! sb (make-mouse 'release 2 9 #:button 'left)))
       (test-assert (not (scroll-box-handle-mouse! sb (make-mouse 'drag 2 0 #:button 'left))))
       ;; A press beside the bar is not on it.
       (test-assert (not (scroll-box-handle-mouse! sb (make-mouse 'press 8 0 #:button 'left)))))
     (let ((sb (make-scroll-box)))
       ;; Content that fits draws no bar, so there is nothing to press.
       (scroll-box-render! sb (make-rect 0 0 10 10) (make-buffer (make-rect 0 0 10 10))
                           (make-scrollable (const 5) (lambda (area buf offset) #f)))
       (test-assert (not (scroll-box-handle-mouse! sb (make-mouse 'press 9 0 #:button 'left))))))


   (test-group "spinner"
     (let ((s (make-spinner %breathing-dot)))
       (test-equal "" (spinner-frame s))
       (test-assert (not (spinner-tick! s 1000)))
       (spinner-start! s 0)
       (test-equal "·" (spinner-frame s))
       (test-assert (not (spinner-tick! s 50)))
       (test-assert (spinner-tick! s 120))
       (test-equal "∙" (spinner-frame s))
       (spinner-start! s 200)
       (test-equal "·" (spinner-frame s))
       (spinner-set-style! s %wave-ripple 300)
       (test-equal "⡀⠂⠂" (spinner-frame s)))
     (test-equal 6 (vector-length (spinner-style-frames %wave-ripple)))
     (test-equal 8 (vector-length (spinner-style-frames %moon-phases))))

   (test-group "message list heights"
     (let ((ml (make-message-list theme)))
       (test-assert (message-list-empty? ml))
       (message-list-push-message! ml 'user "hello")
       (test-equal 1 (message-list-length ml))
       (test-equal 1 (message-list-height ml 40))
       (message-list-push-message! ml 'assistant "world")
       ;; one, a separator, one
       (test-equal 3 (message-list-height ml 40)))
     (let ((ml (make-message-list theme #:display-mode 'compact)))
       (message-list-push-message! ml 'user "this is a long message that would normally wrap")
       (test-equal 1 (message-list-height ml 40)))
     (let ((ml (make-message-list theme)))
       (message-list-push-message! ml 'user "line1\nline2\nline3")
       (let ((full (message-list-height ml 40)))
         (message-list-set-display-mode! ml 'compact)
         (test-assert (> full (message-list-height ml 40)))
         (message-list-set-block-override! ml 0 'full)
         (test-equal full (message-list-height ml 40))
         (message-list-clear-block-override! ml 0)
         (test-equal 1 (message-list-height ml 40))))
     (let ((ml (make-message-list theme #:display-mode 'compact)))
       (message-list-push! ml (make-tool-call-block
                               "read_file"
                               (make-truncated-block theme "1\n2\n3\n4\n5")))
       (test-equal 1 (message-list-height ml 40))))

   (test-group "message list content"
     (let ((ml (make-message-list theme)))
       (message-list-push-message! ml 'user "hello")
       (message-list-push! ml (make-tool-call-block "test" (make-truncated-block theme "out")))
       (message-list-push-message! ml 'assistant "")
       (message-list-set-last-content! ml "updated")
       (test-equal "updated" (message-block-content (message-list-ref ml 2))))
     (let ((ml (make-message-list theme)))
       (message-list-push-message! ml 'user "hello world")
       (message-list-push-message! ml 'assistant "hi there friend")
       (test-equal 5 (message-list-total-word-count ml))))

   (test-group "message list markers"
     (let ((ml (make-message-list theme)))
       (message-list-push-message! ml 'assistant "one two three four" #:marker "● ")
       (message-list-push-message! ml 'user "plain")
       ;; The text wraps at the width left after the marker and hangs under it.
       (test-equal '("● one two" "  three" "  four" "" "┃plain")
         (map string-trim-right
              (render-lines 10 5 (lambda (area buf) (message-list-render! ml area buf)))))
       (test-equal 5 (message-list-height ml 10))
       ;; Scrolled into the block, the marker is gone with its first line.
       (test-equal '("  three" "  four")
         (map string-trim-right
              (render-lines 10 2 (lambda (area buf) (message-list-render! ml area buf 2)))))))

   (test-group "message list drawing is bottom-anchored"
     (let ((ml (make-message-list theme)))
       (message-list-push-message! ml 'user "first")
       (message-list-push-message! ml 'assistant "second")
       (test-equal '("┃first" "" "┃second")
         (map string-trim-right
              (render-lines 10 3 (lambda (area buf) (message-list-render! ml area buf)))))
       ;; Too short for both: the newest is what shows.
       (test-equal "┃second"
         (string-trim-right
          (car (render-lines 10 1 (lambda (area buf) (message-list-render! ml area buf))))))
       ;; Scrolled up by two rows, the oldest comes back.
       (test-equal "┃first"
         (string-trim-right
          (car (render-lines 10 1 (lambda (area buf)
                                    (message-list-render! ml area buf 2))))))))

   (test-group "message list colours the bar by role, or as told"
     (let ((ml (make-message-list theme))
           (buf (make-buffer (make-rect 0 0 10 3))))
       (message-list-push-message! ml 'user "a")
       (message-list-push-message! ml 'assistant "b" #:border 'red)
       (message-list-render! ml (make-rect 0 0 10 3) buf)
       (test-equal (theme-primary theme) (style-fg (buffer-style buf 0 0)))
       (test-equal 'red (style-fg (buffer-style buf 0 2)))))

   (test-group "message list renders todo lists and tool calls"
     (let ((ml (make-message-list theme))
           (todo (make-todo-list-block "Plan" '(("one" . #f) ("two" . #f)))))
       (message-list-push! ml todo)
       (todo-list-block-check! todo 0)
       (let ((lines (map string-trim-right
                         (render-lines 20 3 (lambda (area buf) (message-list-render! ml area buf))))))
         (test-equal '("┃Plan (1/2)" "┃[✓] one" "┃[ ] two") lines)))
     (let ((ml (make-message-list theme)))
       (message-list-push! ml (make-tool-call-block "bash" (make-truncated-block theme "out")))
       (test-equal '(" Tool: bash" "┃out")
         (map string-trim-right
              (render-lines 20 2 (lambda (area buf) (message-list-render! ml area buf)))))))

   (test-group "the empty list shows a hint"
     (let ((lines (render-lines 40 3 (lambda (area buf)
                                       (message-list-render! (make-message-list theme) area buf)))))
       (test-assert (string-contains (cadr lines) "Type a message"))))

   (test-group "a scroll box over a message list"
     (let ((ml (make-message-list theme))
           (sb (make-scroll-box)))
       (for-each (lambda (n) (message-list-push-message! ml 'user (number->string n)))
                 (iota 5))
       (let ((lines (render-lines 10 3 (lambda (area buf)
                                         (scroll-box-render! sb area buf
                                                             (message-list-scrollable ml))))))
         ;; The newest at the bottom, and a scrollbar thumb in the last column.
         (test-equal "┃4" (string-trim-right (substring (caddr lines) 0 9)))
         (test-assert (any (lambda (l) (not (string=? " " (substring l 9 10)))) lines)))))

   (test-group "truncated block"
     (let ((short (make-truncated-block theme "a\nb"))
           (long (make-truncated-block theme "1\n2\n3\n4\n5\n6\n7\n8\n9\n10")))
       (test-equal 5 (truncated-block-max-lines long))
       (test-assert (not (truncated-block-truncated? short 40)))
       (test-assert (truncated-block-truncated? long 40))
       ;; Four lines show and the fifth row is the hint.
       (test-equal 6 (truncated-block-hidden-lines long 40))
       (test-equal 5 (truncated-block-required-height long 40))
       (test-equal 2 (truncated-block-required-height short 40))
       (truncated-block-set-expanded! long #t)
       (test-assert (not (truncated-block-truncated? long 40)))
       (test-equal 10 (truncated-block-required-height long 40)))
     (let ((tb (make-truncated-block theme "1\n2\n3\n4\n5\n6\n7" #:max-lines #f)))
       (test-equal 7 (truncated-block-required-height tb 40)))
     (let ((lines (render-lines 20 5 (lambda (area buf)
                                       (truncated-block-render!
                                        (make-truncated-block theme "1\n2\n3\n4\n5\n6\n7")
                                        area buf)))))
       (test-equal "┃1" (string-trim-right (car lines)))
       (test-equal "┃▸ 3 more lines" (string-trim-right (last lines)))))

   (test-group "status bar"
     (test-equal '(talk plan accept auto ralph talk)
       (let loop ((m 'talk) (n 6) (acc '()))
         (if (zero? n) (reverse acc) (loop (mode-next m) (- n 1) (cons m acc)))))
     (let ((sb (make-status-bar theme #:context-window 1000)))
       (status-bar-set-word-count! sb 250)
       (test-equal 25.0 (status-bar-usage-percent sb))
       (test-equal (theme-primary theme) (status-bar-cycle-mode! sb)))
     (test-equal 0.0 (status-bar-usage-percent (make-status-bar theme #:context-window 0)))
     (test-equal "999" (format-token-count 999))
     (test-equal "1.5k" (format-token-count 1500))
     (test-equal "2.5m" (format-token-count 2500000))
     (let ((sb (make-status-bar theme #:model "glm")))
       (status-bar-set-cwd! sb "~/src/promptui")
       (status-bar-set-latency! sb 42)
       (let ((row (car (render-lines 80 1 (lambda (area buf) (status-bar-render! sb area buf))))))
         (test-assert (string-prefix? "Talk · glm" row))
         (test-assert (string-contains row "~/src/promptui · 42ms · 0 tokens (0.0%)")))
       ;; Narrow: the directory goes first, then the latency.
       (let ((row (car (render-lines 40 1 (lambda (area buf) (status-bar-render! sb area buf))))))
         (test-assert (not (string-contains row "promptui")))
         (test-assert (string-contains row "42ms")))))

   (test-group "streaming text"
     (let ((st (make-streaming-text theme #:now 0)))
       (test-equal "" (streaming-text-revealed st))
       (streaming-text-push! st "hello")
       (test-equal "" (streaming-text-revealed st))
       (test-assert (not (streaming-text-tick! st 10)))
       (test-assert (streaming-text-tick! st 30))
       (test-equal "he" (streaming-text-revealed st))
       (streaming-text-finish! st)
       (test-assert (not (streaming-text-finished? st)))
       (streaming-text-tick! st 60)
       (streaming-text-tick! st 90)
       (test-equal "hello" (streaming-text-revealed st))
       (test-assert (streaming-text-finished? st))
       (test-assert (not (streaming-text-tick! st 200)))
       (streaming-text-push! st " world")
       (test-equal "hello world" (streaming-text-revealed st)))
     (let ((st (make-streaming-text theme #:now 0)))
       (streaming-text-push! st "日本語")
       (streaming-text-tick! st 30)
       (test-equal "日本" (streaming-text-revealed st))
       ;; The cursor trails while text is still to come.
       (test-equal "┃日本▌" (string-trim-right
                            (car (render-lines 10 1 (lambda (area buf)
                                                      (streaming-text-render! st area buf))))))
       (streaming-text-reveal-all! st)
       (test-equal "┃日本語" (string-trim-right
                             (car (render-lines 10 1 (lambda (area buf)
                                                       (streaming-text-render! st area buf))))))
       (test-equal 1 (streaming-text-height st 10))))

   (test-group "border box"
     (let ((bar (make-border-box theme 'left-bar))
           (box (make-border-box theme 'square #:padding (padding-uniform 1))))
       (test-equal 1 (border-box-h-overhead bar))
       (test-equal 0 (border-box-v-overhead bar))
       (test-equal 4 (border-box-h-overhead box))
       (test-equal (make-rect 2 2 6 6) (border-box-inner box (make-rect 0 0 10 10)))
       (test-assert (rect-empty? (border-box-inner box (make-rect 0 0 3 3)))))
     (test-equal '("╭─ Tools ─╮" "│         │" "╰─────────╯")
       (render-lines 11 3 (lambda (area buf)
                            (border-box-render! (make-border-box theme 'rounded #:title "Tools")
                                                area buf))))
     (test-equal '("┌──┐" "└──┘")
       (render-lines 4 2 (lambda (area buf)
                           (border-box-render! (make-border-box theme 'square) area buf)))))

   (test-group "notice"
     (let ((n (make-notice theme)))
       (test-equal 5000 (notice-duration n))
       (test-assert (not (notice-visible? n)))
       (test-equal 0.0 (notice-progress n 0))
       (notice-show! n 'warn "careful" 0)
       (test-assert (notice-visible? n))
       (test-equal 0.5 (notice-progress n 2500))
       (notice-tick! n 4999)
       (test-assert (notice-visible? n))
       (notice-tick! n 5000)
       (test-assert (not (notice-visible? n)))
       (notice-show! n 'info "one" 0)
       (notice-show! n 'error "two" 0)
       (let ((lines (render-lines 30 3 (lambda (area buf) (notice-render! n area buf 0)))))
         (test-assert (string-contains (cadr lines) "ERR  two"))
         (test-assert (not (string-contains (cadr lines) "one"))))
       (notice-dismiss! n)
       (test-equal '("     ")
         (render-lines 5 1 (lambda (area buf) (notice-render! n area buf 0)))))
     (test-equal "WRN" (notice-level-label 'warn))
     (test-equal (theme-error theme) (notice-level-color 'error theme)))

   (test-group "breadcrumb"
     (let ((bc (make-breadcrumb theme)))
       (test-equal 0 (breadcrumb-height bc))
       (breadcrumb-push! bc (make-breadcrumb-segment "main" 'blue))
       (test-equal 0 (breadcrumb-height bc))
       (breadcrumb-push! bc (make-breadcrumb-segment "research" 'green))
       (test-equal 1 (breadcrumb-height bc))
       (test-equal '(#f #t) (map breadcrumb-segment-active? (breadcrumb-segments bc)))
       (test-equal "main › research"
         (string-trim-right
          (car (render-lines 30 1 (lambda (area buf) (breadcrumb-render! bc area buf))))))
       (breadcrumb-pop! bc)
       (test-equal '(#t) (map breadcrumb-segment-active? (breadcrumb-segments bc)))
       (breadcrumb-set-padding! bc (padding-uniform 1))
       (breadcrumb-push! bc (make-breadcrumb-segment "x" 'red))
       (test-equal 3 (breadcrumb-height bc))))

   (test-group "todo list"
     (let ((tl (make-todo-list theme "Tasks")))
       (test-equal 1 (todo-list-required-height tl))
       (todo-list-push! tl (make-todo-item "schema" #t))
       (todo-list-push! tl (make-todo-item "deploy"))
       (test-equal 3 (todo-list-required-height tl))
       (test-equal 1 (todo-list-done-count tl))
       (test-equal '("┃Tasks (1/2)" "┃[✓] schema" "┃[ ] deploy")
         (map string-trim-right
              (render-lines 20 3 (lambda (area buf) (todo-list-render! tl area buf)))))
       (test-equal 3 (scrollable-height (todo-list-scrollable tl) 20))
       (test-equal '()
         (render-lines 0 0 (lambda (area buf) (todo-list-render! tl area buf))))))))
