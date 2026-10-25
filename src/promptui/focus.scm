;;; The focus stack: which component receives the next key.
;;;
;;; The stack holds identifiers, usually symbols, and the application dispatches a
;;; key to whichever component the top one names. The base identifier, the text
;;; field in a chat interface, is never popped, so there is always somewhere for a
;;; key to go.
;;;
;;; A focusable component answers a key with one of three actions: consumed, pop
;;; (take me off the stack, for example on Escape), or ignored (let the application
;;; treat it as a global shortcut).

(define-module (promptui focus)
  #:use-module (srfi srfi-9)
  #:export (make-focus-stack
            focus-stack?
            focus-top
            focus-push!
            focus-pop!
            focus-contains?
            focus-depth
            focus-ids))

(define-record-type <focus-stack>
  (%make-focus-stack ids)
  focus-stack?
  ;; Top first.
  (ids focus-ids set-focus-ids!))

(define* (make-focus-stack #:optional (base 'text-field))
  (%make-focus-stack (list base)))

(define (focus-top stack)
  (car (focus-ids stack)))

(define (focus-push! stack id)
  "Put ID on top. False when it is already there."
  (if (equal? (focus-top stack) id)
      #f
      (begin (set-focus-ids! stack (cons id (focus-ids stack))) #t)))

(define (focus-pop! stack)
  "Remove and return the top identifier, or #f when only the base is left."
  (let ((ids (focus-ids stack)))
    (if (null? (cdr ids))
        #f
        (begin (set-focus-ids! stack (cdr ids)) (car ids)))))

(define (focus-contains? stack id)
  (and (member id (focus-ids stack)) #t))

(define (focus-depth stack)
  (length (focus-ids stack)))
