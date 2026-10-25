;;; A panel over every running agent, in three sizes, each with its own transcript.
;;;
;;; Collapsed is one line: a status dot per agent, a count and the token totals.
;;; Summary is a bordered list of a few agents with checkboxes. Full replaces the
;;; message area with the whole tree; Enter there opens one agent's own message list,
;;; and Escape comes back out. Cycling goes Collapsed, Summary, Full, Collapsed.
;;;
;;; With one agent or none there is nothing to overview, and the panel takes no room.

(define-module (promptui widgets agent-overview)
  #:use-module (promptui width)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (promptui theme)
  #:use-module (promptui padding)
  #:use-module (promptui widgets common)
  #:use-module (promptui widgets agent-scope)
  #:use-module (promptui widgets border-box)
  #:use-module (promptui widgets message-list)
  #:use-module (promptui widgets scroll-box)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-agent-entry
            agent-entry?
            agent-entry-id
            agent-entry-label
            agent-entry-color
            agent-entry-status
            set-agent-entry-status!
            agent-entry-duration
            set-agent-entry-duration!
            agent-entry-input-tokens
            set-agent-entry-input-tokens!
            agent-entry-output-tokens
            set-agent-entry-output-tokens!
            agent-entry-activity
            set-agent-entry-activity!
            agent-entry-checked?
            set-agent-entry-checked!
            agent-entry-depth
            make-agent-overview
            agent-overview?
            agent-overview-push!
            agent-overview-push-message!
            agent-overview-agents
            agent-overview-agent
            agent-overview-count
            agent-overview-focused
            agent-overview-focus-up!
            agent-overview-focus-down!
            agent-overview-toggle-focused!
            agent-overview-checked-ids
            agent-overview-checked-count
            agent-overview-mode
            agent-overview-cycle-mode!
            agent-overview-viewing
            agent-overview-set-max-compact-rows!
            agent-overview-set-padding!
            agent-overview-total-input-tokens
            agent-overview-total-output-tokens
            agent-overview-required-height
            agent-overview-handle-input!
            agent-overview-on-focus!
            agent-overview-on-blur!
            agent-overview-render!
            agent-overview-render-full!))

(define %checked "[×]")        ; [×]
(define %unchecked "[ ]")
(define %tree-branch "├─")
(define %tree-last "└─")

;;; Entries

(define-record-type <agent-entry>
  (%make-agent-entry id label color status duration input-tokens output-tokens
                     activity checked depth)
  agent-entry?
  (id            agent-entry-id)
  (label         agent-entry-label)
  (color         agent-entry-color)
  (status        agent-entry-status set-agent-entry-status!)
  ;; Seconds, or #f.
  (duration      agent-entry-duration set-agent-entry-duration!)
  (input-tokens  agent-entry-input-tokens set-agent-entry-input-tokens!)
  (output-tokens agent-entry-output-tokens set-agent-entry-output-tokens!)
  (activity      agent-entry-activity set-agent-entry-activity!)
  (checked       agent-entry-checked? set-agent-entry-checked!)
  (depth         agent-entry-depth))

(define* (make-agent-entry id label #:key (color 'default) (status 'active)
                           (duration #f) (input-tokens 0) (output-tokens 0)
                           (activity "") (checked #t) (depth 0))
  (%make-agent-entry id label color status duration input-tokens output-tokens
                     activity checked depth))

;;; The overview

(define-record-type <agent-overview>
  (%make-agent-overview theme padding agents focused mode inner max-compact-rows
                        messages scrolls)
  agent-overview?
  (theme            ao-theme)
  (padding          ao-padding agent-overview-set-padding!)
  (agents           agent-overview-agents set-agents!)
  (focused          agent-overview-focused set-focused!)
  ;; collapsed, summary or full.
  (mode             agent-overview-mode set-mode!)
  ;; Within full: tree, or the index of the agent whose messages are open.
  (inner            ao-inner set-inner!)
  (max-compact-rows ao-max-compact-rows agent-overview-set-max-compact-rows!)
  ;; One message list and one scroll box per agent, in the same order.
  (messages         ao-messages set-messages!)
  (scrolls          ao-scrolls set-scrolls!))

(define* (make-agent-overview theme #:key (padding (padding-uniform 0))
                              (max-compact-rows 3))
  (%make-agent-overview theme padding '() 0 'collapsed 'tree max-compact-rows '() '()))

(define (agent-overview-push! ao entry)
  "Add an agent, with a message list of its own."
  (set-agents! ao (append (agent-overview-agents ao) (list entry)))
  (set-messages! ao (append (ao-messages ao)
                            (list (make-message-list (ao-theme ao)
                                                     #:padding (padding-uniform 1)))))
  (set-scrolls! ao (append (ao-scrolls ao) (list (make-scroll-box))))
  entry)

(define (agent-overview-push-message! ao index role content)
  "Add a message to agent INDEX's own transcript, scrolled to show it."
  (when (< -1 index (agent-overview-count ao))
    (message-list-push-message! (list-ref (ao-messages ao) index) role content)
    (scroll-box-scroll-to-bottom! (list-ref (ao-scrolls ao) index))))

(define (agent-overview-count ao) (length (agent-overview-agents ao)))

(define (agent-overview-agent ao index)
  (and (< -1 index (agent-overview-count ao))
       (list-ref (agent-overview-agents ao) index)))

(define (agent-overview-focus-up! ao)
  (when (> (agent-overview-focused ao) 0)
    (set-focused! ao (- (agent-overview-focused ao) 1))))

(define (agent-overview-focus-down! ao)
  (when (< (+ 1 (agent-overview-focused ao)) (agent-overview-count ao))
    (set-focused! ao (+ 1 (agent-overview-focused ao)))))

(define (agent-overview-toggle-focused! ao)
  (let ((a (agent-overview-agent ao (agent-overview-focused ao))))
    (when a (set-agent-entry-checked! a (not (agent-entry-checked? a))))))

(define (agent-overview-checked-ids ao)
  (map agent-entry-id (filter agent-entry-checked? (agent-overview-agents ao))))

(define (agent-overview-checked-count ao)
  (count agent-entry-checked? (agent-overview-agents ao)))

(define (agent-overview-cycle-mode! ao)
  "Collapsed, Summary, Full, and round again. Returns the new mode."
  (set-mode! ao (case (agent-overview-mode ao)
                  ((collapsed) 'summary)
                  ((summary) (set-inner! ao 'tree) 'full)
                  (else 'collapsed)))
  (agent-overview-mode ao))

(define (agent-overview-viewing ao)
  "The index of the agent whose messages Full mode is showing, or #f."
  (and (eq? (agent-overview-mode ao) 'full)
       (integer? (ao-inner ao))
       (ao-inner ao)))

(define (agent-overview-total-input-tokens ao)
  (apply + (map agent-entry-input-tokens (agent-overview-agents ao))))

(define (agent-overview-total-output-tokens ao)
  (apply + (map agent-entry-output-tokens (agent-overview-agents ao))))

(define (agent-overview-required-height ao)
  "Rows for the header forms. Full mode draws into the message area instead, so it
takes none here."
  (if (<= (agent-overview-count ao) 1)
      0
      (case (agent-overview-mode ao)
        ((collapsed) 1)
        ((summary) (+ 2 (min (agent-overview-count ao) (ao-max-compact-rows ao))
                      (padding-vertical (ao-padding ao))))
        (else 0))))

;;; Input

(define (agent-overview-handle-input! ao input)
  "Returns consumed, pop or ignored."
  (if (integer? (ao-inner ao))
      (let ((sb (list-ref (ao-scrolls ao) (ao-inner ao))))
        (case input
          ((esc) (set-inner! ao 'tree) 'consumed)
          ((pageup) (scroll-box-page-up! sb) 'consumed)
          ((pagedown) (scroll-box-page-down! sb) 'consumed)
          (else 'ignored)))
      (cond
       ((eq? input 'esc) (set-mode! ao 'summary) 'pop)
       ((eq? input 'up) (agent-overview-focus-up! ao) 'consumed)
       ((eq? input 'down) (agent-overview-focus-down! ao) 'consumed)
       ((eq? input 'enter)
        (when (> (agent-overview-count ao) 0)
          (set-inner! ao (agent-overview-focused ao)))
        'consumed)
       ((eqv? input #\space) (agent-overview-toggle-focused! ao) 'consumed)
       (else 'ignored))))

(define (agent-overview-on-focus! ao) (set-inner! ao 'tree))
(define (agent-overview-on-blur! ao) #f)

;;; Drawing

(define (pad-label label width)
  (string-pad-width label width))

(define (stats-text entry)
  "\" 12s · ↑2.1k ↓940 \", or #f when there is nothing to say."
  (let ((parts (append
                (if (agent-entry-duration entry)
                    (let ((s (inexact->exact (floor (agent-entry-duration entry)))))
                      (list (if (< s 60) (format #f "~as" s) (format #f "~am" (quotient s 60)))))
                    '())
                (if (or (> (agent-entry-input-tokens entry) 0)
                        (> (agent-entry-output-tokens entry) 0))
                    (list (format #f "↑~a ↓~a"
                                  (format-tokens (agent-entry-input-tokens entry))
                                  (format-tokens (agent-entry-output-tokens entry))))
                    '()))))
    (and (pair? parts)
         (string-append " " (string-join parts " · ") " "))))

(define (draw-stats! buf entry x y x-max style)
  (let ((stats (stats-text entry)))
    (when stats
      (let ((start (- x-max (string-width stats))))
        (when (> start x)
          (draw-text! buf start y x-max stats style))))))

(define (agent-overview-render! ao area buf)
  "The header forms: Collapsed and Summary. Full is drawn by agent-overview-render-full!."
  (unless (or (rect-empty? area) (<= (agent-overview-count ao) 1))
    (case (agent-overview-mode ao)
      ((collapsed) (render-collapsed ao area buf))
      ((summary) (render-summary ao area buf))
      (else #f)))
  buf)

(define (render-collapsed ao area buf)
  (let* ((theme (ao-theme ao))
         (pad (ao-padding ao))
         (y (rect-y area))
         (x-max (- (rect-right area) (padding-right pad)))
         (muted (theme-muted-style theme)))
    (fill-background! buf (rect-row area 0) (make-style #:bg (theme-panel-bg theme)))
    (let* ((x (fold (lambda (a x)
                      (draw-text! buf x y x-max (agent-status-icon (agent-entry-status a))
                                  (theme-fg-style theme
                                                  (agent-status-color (agent-entry-status a)
                                                                      theme))))
                    (+ (rect-x area) (padding-left pad))
                    (agent-overview-agents ao)))
           (x (draw-text! buf x y x-max
                          (format #f "  ~a agents" (agent-overview-count ao)) muted))
           (x (draw-text! buf x y x-max
                          (format #f "  ↑~a ↓~a"
                                  (format-tokens (agent-overview-total-input-tokens ao))
                                  (format-tokens (agent-overview-total-output-tokens ao)))
                          muted)))
      (draw-text! buf x y x-max "  Ctrl+A" muted))))

(define (render-summary ao area buf)
  (let* ((theme (ao-theme ao))
         (box (make-border-box theme 'rounded
                               #:title (format #f "Agents (~a)" (agent-overview-count ao))))
         (inner (border-box-inner box area))
         (max-rows (ao-max-compact-rows ao))
         (overflow? (> (agent-overview-count ao) max-rows))
         (rows (if overflow? (max 0 (- max-rows 1)) (agent-overview-count ao))))
    (border-box-render! box area buf)
    (unless (rect-empty? inner)
      (let loop ((agents (agent-overview-agents ao)) (i 0))
        (when (and (pair? agents) (< i rows) (< (+ (rect-y inner) i) (rect-bottom inner)))
          (let* ((a (car agents))
                 (y (+ (rect-y inner) i))
                 (focused? (= i (agent-overview-focused ao)))
                 (row-bg (if focused? (theme-element-bg theme) (theme-panel-bg theme)))
                 (bold (if focused? '(bold) '()))
                 (x-max (rect-right inner)))
            (when focused?
              (buffer-fill! buf (rect-row inner i) " " (make-style #:bg row-bg)))
            (let* ((x (min x-max (+ (rect-x inner) (* 2 (agent-entry-depth a)) 1)))
                   (x (draw-text! buf x y x-max
                                  (if (agent-entry-checked? a) %checked %unchecked)
                                  (make-style #:fg (if focused? (theme-text theme)
                                                       (theme-text-muted theme))
                                              #:bg row-bg #:modifiers bold)))
                   (x (min x-max (+ x 1)))
                   (x (draw-text! buf x y x-max (agent-status-icon (agent-entry-status a))
                                  (make-style #:fg (agent-status-color (agent-entry-status a)
                                                                       theme)
                                              #:bg row-bg)))
                   (x (min x-max (+ x 1)))
                   (x (draw-text! buf x y x-max
                                  (pad-label (agent-entry-label a) (min 12 (- x-max x)))
                                  (make-style #:fg (agent-entry-color a) #:bg row-bg
                                              #:modifiers bold)))
                   (muted (make-style #:fg (theme-text-muted theme) #:bg row-bg))
                   (x (if (and (not (string-null? (agent-entry-activity a))) (< x x-max))
                          (draw-text! buf x y x-max
                                      (pad-label (agent-entry-activity a) (min 16 (- x-max x)))
                                      muted)
                          x)))
              (draw-stats! buf a x y x-max muted))
            (loop (cdr agents) (+ i 1)))))
      (when overflow?
        (let ((y (+ (rect-y inner) rows)))
          (when (< y (rect-bottom inner))
            (draw-text! buf (+ (rect-x inner) 1) y (rect-right inner)
                        (format #f "+~a more  Ctrl+A ▸" (- (agent-overview-count ao) rows))
                        (theme-muted-style theme))))))))

(define (agent-overview-render-full! ao area buf)
  "Full mode, drawn into the area the message list would otherwise use."
  (unless (rect-empty? area)
    (if (integer? (ao-inner ao))
        (render-agent-messages ao area buf (ao-inner ao))
        (render-tree ao area buf)))
  buf)

(define (render-tree ao area buf)
  (let* ((theme (ao-theme ao))
         (pad (ao-padding ao))
         (agents (agent-overview-agents ao))
         (n (length agents)))
    (fill-background! buf area (make-style #:bg (theme-panel-bg theme)))
    (let loop ((rest agents) (i 0))
      (when (and (pair? rest) (< i (rect-height area)))
        (let* ((a (car rest))
               (y (+ (rect-y area) i))
               (focused? (= i (agent-overview-focused ao)))
               (row-bg (if focused? (theme-element-bg theme) (theme-panel-bg theme)))
               (x-max (- (rect-right area) (padding-right pad)))
               (muted (make-style #:fg (theme-text-muted theme) #:bg row-bg)))
          (when focused?
            (buffer-fill! buf (rect-row area i) " " (make-style #:bg row-bg)))
          (let* ((x (+ (rect-x area) (padding-left pad)))
                 (x (if (> (agent-entry-depth a) 0)
                        (let* ((x (min x-max (+ x (* 2 (- (agent-entry-depth a) 1)))))
                               ;; The last child closes its branch: the next row is
                               ;; shallower, or there is no next row.
                               (last? (or (= (+ i 1) n)
                                          (< (agent-entry-depth (cadr rest))
                                             (agent-entry-depth a))))
                               (x (draw-text! buf x y x-max (if last? %tree-last %tree-branch)
                                              (make-style #:fg (theme-border theme) #:bg row-bg))))
                          (min x-max (+ x 1)))
                        x))
                 (x (draw-text! buf x y x-max (agent-status-icon (agent-entry-status a))
                                (make-style #:fg (agent-status-color (agent-entry-status a) theme)
                                            #:bg row-bg)))
                 (x (min x-max (+ x 1)))
                 (x (draw-text! buf x y x-max
                                (pad-label (agent-entry-label a) (min 14 (- x-max x)))
                                (make-style #:fg (agent-entry-color a) #:bg row-bg
                                            #:modifiers (if focused? '(bold) '()))))
                 (x (if (and (not (string-null? (agent-entry-activity a))) (< x x-max))
                        (draw-text! buf x y x-max
                                    (pad-label (agent-entry-activity a) (min 20 (- x-max x)))
                                    muted)
                        x)))
            (draw-stats! buf a x y x-max muted)))
        (loop (cdr rest) (+ i 1))))))

(define (render-agent-messages ao area buf index)
  (let* ((theme (ao-theme ao))
         (pad (ao-padding ao))
         (a (agent-overview-agent ao index))
         (y (rect-y area))
         (x-max (rect-right area))
         (muted (theme-muted-style theme)))
    (fill-background! buf area (make-style #:bg (theme-panel-bg theme)))
    (let* ((x (draw-text! buf (+ (rect-x area) (padding-left pad)) y x-max "Agents" muted))
           (x (draw-text! buf x y x-max " › " muted)))
      (draw-text! buf x y x-max (if a (agent-entry-label a) "")
                  (theme-fg-style theme (if a (agent-entry-color a) (theme-text-muted theme)))))
    (let ((below (rect-inset area #:top 1)))
      (when (and a (not (rect-empty? below)))
        (scroll-box-render! (list-ref (ao-scrolls ao) index) below buf
                            (message-list-scrollable (list-ref (ao-messages ao) index)))))))
