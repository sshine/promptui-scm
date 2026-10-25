;;; A read-only checklist showing how far a plan has come.
;;;
;;;   ┃ Tasks (2/4)
;;;   ┃ [✓] Set up database schema
;;;   ┃ [✓] Write migration script
;;;   ┃ [ ] Run integration tests
;;;   ┃ [ ] Deploy to staging

(define-module (promptui widgets todo-list)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (promptui theme)
  #:use-module (promptui padding)
  #:use-module (promptui widgets common)
  #:use-module (promptui widgets scroll-box)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-todo-item
            todo-item?
            todo-item-label
            todo-item-done?
            make-todo-list
            todo-list?
            todo-list-title
            todo-list-items
            todo-list-set-items!
            todo-list-push!
            todo-list-done-count
            todo-list-total-count
            todo-list-set-padding!
            todo-list-required-height
            todo-list-render!
            todo-list-scrollable))

(define-record-type <todo-item>
  (%make-todo-item label done)
  todo-item?
  (label todo-item-label)
  (done  todo-item-done?))

(define* (make-todo-item label #:optional (done #f))
  (%make-todo-item label done))

(define-record-type <todo-list>
  (%make-todo-list theme padding title items)
  todo-list?
  (theme   tl-theme)
  (padding tl-padding todo-list-set-padding!)
  (title   todo-list-title)
  (items   todo-list-items todo-list-set-items!))

(define* (make-todo-list theme title #:key (items '()) (padding (padding-uniform 0)))
  (%make-todo-list theme padding title items))

(define (todo-list-push! tl item)
  (todo-list-set-items! tl (append (todo-list-items tl) (list item))))

(define (todo-list-done-count tl) (count todo-item-done? (todo-list-items tl)))
(define (todo-list-total-count tl) (length (todo-list-items tl)))

(define (todo-list-required-height tl)
  (+ 1 (todo-list-total-count tl) (padding-vertical (tl-padding tl))))

(define (todo-list-render! tl area buf)
  (unless (rect-empty? area)
    (let* ((theme (tl-theme tl))
           (pad (tl-padding tl))
           (text-x (+ (rect-x area) %border-width (padding-left pad)))
           (x-max (- (rect-right area) (padding-right pad)))
           (y-max (- (rect-bottom area) (padding-bottom pad))))
      (fill-background! buf area (make-style #:bg (theme-panel-bg theme)))
      (draw-left-bar! buf area (theme-fg-style theme (theme-secondary theme)))
      (when (> x-max text-x)
        (let ((y (+ (rect-y area) (padding-top pad))))
          (when (< y y-max)
            (draw-text! buf text-x y x-max
                        (format #f "~a (~a/~a)" (todo-list-title tl)
                                (todo-list-done-count tl) (todo-list-total-count tl))
                        (theme-text-style theme))
            (let loop ((items (todo-list-items tl)) (y (+ y 1)))
              (when (and (pair? items) (< y y-max))
                (let* ((done? (todo-item-done? (car items)))
                       (x (draw-text! buf text-x y x-max (if done? "[✓]" "[ ]")
                                      (if done?
                                          (theme-fg-style theme (theme-success theme))
                                          (theme-muted-style theme)))))
                  (draw-text! buf (min x-max (+ x 1)) y x-max (todo-item-label (car items))
                              (if done? (theme-muted-style theme) (theme-text-style theme)))
                  (loop (cdr items) (+ y 1))))))))))
  buf)

(define (todo-list-scrollable tl)
  (make-scrollable (lambda (width) (todo-list-required-height tl))
                   (lambda (area buf offset) (todo-list-render! tl area buf))))
