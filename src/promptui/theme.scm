;;; The semantic palette every widget draws from.
;;;
;;; Widgets never name a colour; they ask the theme for its primary accent or its
;;; muted text, so one theme value restyles all of them. Two presets ship:
;;; %opencode-theme carries promptui-rs's defaults, OpenCode's dark palette in 24-bit
;;; colour, and %terminal-theme keeps the terminal's own background and draws with
;;; the sixteen ANSI colours, so it follows whatever palette the user has chosen.

(define-module (promptui theme)
  #:use-module (promptui style)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-theme
            theme?
            theme-bg
            theme-panel-bg
            theme-element-bg
            theme-text
            theme-text-muted
            theme-primary
            theme-secondary
            theme-accent
            theme-error
            theme-warning
            theme-success
            theme-info
            theme-border
            theme-fields
            theme-ref
            theme-with
            %opencode-theme
            %terminal-theme
            theme-text-style
            theme-muted-style
            theme-input-style
            theme-fg-style
            theme-border-focused-style
            theme-border-unfocused-style))

(define-immutable-record-type <theme>
  (%make-theme bg panel-bg element-bg text text-muted primary secondary accent
               error warning success info border)
  theme?
  (bg          theme-bg)
  (panel-bg    theme-panel-bg)
  (element-bg  theme-element-bg)
  (text        theme-text)
  (text-muted  theme-text-muted)
  (primary     theme-primary)
  (secondary   theme-secondary)
  (accent      theme-accent)
  (error       theme-error)
  (warning     theme-warning)
  (success     theme-success)
  (info        theme-info)
  (border      theme-border))

(define* (make-theme #:key
                     (bg (rgb 10 10 10))
                     (panel-bg (rgb 20 20 20))
                     (element-bg (rgb 30 30 30))
                     (text (rgb 238 238 238))
                     (text-muted (rgb 128 128 128))
                     (primary (rgb 250 178 131))     ; orange
                     (secondary (rgb 92 156 245))    ; blue
                     (accent (rgb 157 124 216))      ; purple
                     (error (rgb 224 108 117))       ; red
                     (warning (rgb 245 167 66))      ; orange
                     (success (rgb 127 216 143))     ; green
                     (info (rgb 86 182 194))         ; cyan
                     (border (rgb 48 48 48)))
  (%make-theme bg panel-bg element-bg text text-muted primary secondary accent
               error warning success info border))

(define %opencode-theme (make-theme))

(define %terminal-theme
  (make-theme #:bg 'default
              #:panel-bg 'default
              #:element-bg 'default
              #:text 'default
              #:text-muted 'bright-black
              #:primary 'yellow
              #:secondary 'blue
              #:accent 'magenta
              #:error 'red
              #:warning 'yellow
              #:success 'green
              #:info 'cyan
              #:border 'bright-black))

(define theme-fields
  '(bg panel-bg element-bg text text-muted primary secondary accent
    error warning success info border))

(define %accessors
  (list (cons 'bg theme-bg) (cons 'panel-bg theme-panel-bg)
        (cons 'element-bg theme-element-bg) (cons 'text theme-text)
        (cons 'text-muted theme-text-muted) (cons 'primary theme-primary)
        (cons 'secondary theme-secondary) (cons 'accent theme-accent)
        (cons 'error theme-error) (cons 'warning theme-warning)
        (cons 'success theme-success) (cons 'info theme-info)
        (cons 'border theme-border)))

(define (theme-ref theme field)
  "The colour of FIELD, a symbol from theme-fields, or #f for an unknown field."
  (let ((get (assq-ref %accessors field)))
    (and get (get theme))))

(define (theme-with theme . overrides)
  "THEME with the colours in OVERRIDES replaced, given as alternating field symbols
and colours: (theme-with t 'primary 'cyan 'border 'blue)."
  (let loop ((rest overrides)
             (fields (map (lambda (f) (cons f (theme-ref theme f))) theme-fields)))
    (if (or (null? rest) (null? (cdr rest)))
        (apply %make-theme (map cdr fields))
        (loop (cddr rest)
              (map (lambda (kv)
                     (if (eq? (car kv) (car rest)) (cons (car kv) (cadr rest)) kv))
                   fields)))))

;;; Styles

(define (theme-text-style t)
  "Primary text on the panel background."
  (make-style #:fg (theme-text t) #:bg (theme-panel-bg t)))

(define (theme-muted-style t)
  "Muted text on the panel background."
  (make-style #:fg (theme-text-muted t) #:bg (theme-panel-bg t)))

(define (theme-input-style t)
  "Text inside an input field."
  (make-style #:fg (theme-text t) #:bg (theme-element-bg t)))

(define (theme-fg-style t color)
  "COLOR on the panel background."
  (make-style #:fg color #:bg (theme-panel-bg t)))

(define (theme-border-focused-style t)
  (make-style #:fg (theme-primary t) #:bg (theme-element-bg t)))

(define (theme-border-unfocused-style t)
  (make-style #:fg (theme-border t) #:bg (theme-element-bg t)))
