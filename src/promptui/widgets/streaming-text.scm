;;; Text revealed a few characters at a time, the way a streamed reply arrives.
;;;
;;; Text is pushed in whenever it arrives, and each tick reveals a few more of the
;;; characters already pushed, at a steady rate, so bursty input reads as an even
;;; stream. A block cursor trails the text until everything has been shown.

(define-module (promptui widgets streaming-text)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (promptui text)
  #:use-module (promptui wrap)
  #:use-module (promptui theme)
  #:use-module (promptui padding)
  #:use-module (promptui widgets common)
  #:use-module (srfi srfi-9)
  #:export (make-streaming-text
            streaming-text?
            streaming-text-push!
            streaming-text-tick!
            streaming-text-finish!
            streaming-text-reveal-all!
            streaming-text-finished?
            streaming-text-revealed
            streaming-text-set-chars-per-tick!
            streaming-text-set-interval!
            streaming-text-set-padding!
            streaming-text-height
            streaming-text-render!))

(define %stream-cursor "▌")    ; ▌

(define-record-type <streaming-text>
  (%make-streaming-text buffer revealed chars-per-tick interval last-tick done
                        theme padding)
  streaming-text?
  (buffer         st-buffer set-st-buffer!)
  ;; Characters shown so far.
  (revealed       st-revealed set-st-revealed!)
  (chars-per-tick st-chars-per-tick set-st-chars-per-tick!)
  ;; Milliseconds between ticks.
  (interval       st-interval streaming-text-set-interval!)
  (last-tick      st-last-tick set-st-last-tick!)
  (done           st-done set-st-done!)
  (theme          st-theme)
  (padding        st-padding streaming-text-set-padding!))

(define* (make-streaming-text theme #:key (chars-per-tick 2) (interval 30)
                              (padding (padding-uniform 0)) (now (now-ms)))
  (%make-streaming-text "" 0 chars-per-tick interval now #f theme padding))

(define (streaming-text-push! st text)
  "Append TEXT. Once the stream is finished it is shown at once."
  (set-st-buffer! st (string-append (st-buffer st) text))
  (when (st-done st)
    (set-st-revealed! st (string-length (st-buffer st)))))

(define* (streaming-text-tick! st #:optional (now (now-ms)))
  "Reveal more if the interval has passed. True when the visible text changed."
  (let ((total (string-length (st-buffer st))))
    (and (< (st-revealed st) total)
         (>= (- now (st-last-tick st)) (st-interval st))
         (begin
           (set-st-last-tick! st now)
           (set-st-revealed! st (min total (+ (st-revealed st) (st-chars-per-tick st))))
           #t))))

(define (streaming-text-finish! st)
  "No more text is coming. What is buffered still reveals at the usual rate."
  (set-st-done! st #t))

(define (streaming-text-reveal-all! st)
  (set-st-revealed! st (string-length (st-buffer st))))

(define (streaming-text-finished? st)
  (and (st-done st) (>= (st-revealed st) (string-length (st-buffer st)))))

(define (streaming-text-revealed st)
  (substring (st-buffer st) 0 (st-revealed st)))

(define (streaming-text-set-chars-per-tick! st n)
  (set-st-chars-per-tick! st (max 1 n)))

(define (display-text st)
  (if (>= (st-revealed st) (string-length (st-buffer st)))
      (streaming-text-revealed st)
      (string-append (streaming-text-revealed st) %stream-cursor)))

(define (streaming-text-height st width)
  (let* ((pad (st-padding st))
         (text-width (- width %border-width (padding-horizontal pad))))
    (if (<= text-width 0)
        (padding-vertical pad)
        (+ (max 1 (count-wrapped-text (display-text st) text-width))
           (padding-vertical pad)))))

(define (streaming-text-render! st area buf)
  (unless (rect-empty? area)
    (let* ((theme (st-theme st))
           (pad (st-padding st))
           (text-area (rect-inset area
                                  #:left (+ %border-width (padding-left pad))
                                  #:right (padding-right pad)
                                  #:top (padding-top pad)
                                  #:bottom (padding-bottom pad))))
      (fill-background! buf area (make-style #:bg (theme-panel-bg theme)))
      (draw-left-bar! buf area (theme-fg-style theme (theme-secondary theme)))
      (unless (rect-empty? text-area)
        (render-paragraph! buf text-area (display-text st)
                           #:style (theme-text-style theme)))))
  buf)
