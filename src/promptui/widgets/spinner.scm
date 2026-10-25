;;; An activity indicator that cycles through frames at a fixed interval.
;;;
;;; The spinner does not own a timer. The application calls spinner-tick! as often
;;; as it likes, and the frame advances only once the interval has passed, so the
;;; animation runs at its own speed whatever the redraw rate.

(define-module (promptui widgets spinner)
  #:use-module (promptui widgets common)
  #:use-module (srfi srfi-9)
  #:export (make-spinner-style
            spinner-style?
            spinner-style-frames
            spinner-style-interval
            %moon-phases
            %breathing-dot
            %wave-ripple
            %ascii-spinner
            make-spinner
            spinner?
            spinner-start!
            spinner-stop!
            spinner-active?
            spinner-tick!
            spinner-frame
            spinner-set-style!))

(define-record-type <spinner-style>
  (make-spinner-style frames interval)
  spinner-style?
  ;; A vector of strings.
  (frames   spinner-style-frames)
  ;; Milliseconds between frames.
  (interval spinner-style-interval))

;; Moon waxing and waning. Each frame carries a trailing space so that the wide
;; emoji and what follows it stay apart.
(define %moon-phases
  (make-spinner-style
   #("\U01F311 " "\U01F312 " "\U01F313 " "\U01F314 "
     "\U01F315 " "\U01F316 " "\U01F317 " "\U01F318 ")
   150))

;; A dot that swells and shrinks. One cell wide.
(define %breathing-dot
  (make-spinner-style #("·" "∙" "•" "●" "◉" "●" "•" "∙") 120))

;; A sine wave across three braille cells, a third of a period apart.
(define %wave-ripple
  (make-spinner-style #("⡀⠂⠂" "⠄⠁⠄" "⠂⠂⡀" "⠁⠄⠄" "⠂⡀⠂" "⠄⠄⠁") 100))

;; For a terminal or a font without the glyphs above.
(define %ascii-spinner
  (make-spinner-style #("|" "/" "-" "\\") 120))

(define-record-type <spinner>
  (%make-spinner style frame last-tick active)
  spinner?
  (style     spinner-style set-style!)
  (frame     spinner-frame-index set-frame!)
  (last-tick spinner-last-tick set-last-tick!)
  (active    spinner-active? set-active!))

(define* (make-spinner #:optional (style %breathing-dot))
  (%make-spinner style 0 0 #f))

(define* (spinner-start! s #:optional (now (now-ms)))
  "Start from the first frame."
  (set-active! s #t)
  (set-frame! s 0)
  (set-last-tick! s now))

(define (spinner-stop! s)
  (set-active! s #f))

(define* (spinner-tick! s #:optional (now (now-ms)))
  "Advance a frame if the interval has passed. True when the frame changed."
  (and (spinner-active? s)
       (>= (- now (spinner-last-tick s)) (spinner-style-interval (spinner-style s)))
       (let ((frames (spinner-style-frames (spinner-style s))))
         (set-frame! s (modulo (+ 1 (spinner-frame-index s)) (vector-length frames)))
         (set-last-tick! s now)
         #t)))

(define (spinner-frame s)
  "The current frame, or the empty string when stopped."
  (if (spinner-active? s)
      (vector-ref (spinner-style-frames (spinner-style s)) (spinner-frame-index s))
      ""))

(define* (spinner-set-style! s style #:optional (now (now-ms)))
  "Switch animations, starting the new one from its first frame."
  (set-style! s style)
  (set-frame! s 0)
  (set-last-tick! s now))
