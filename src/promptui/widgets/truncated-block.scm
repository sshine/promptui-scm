;;; A block of text that shows only its first few lines until it is expanded.
;;;
;;; Whether it is expanded is decided outside; the block only honours the flag. When
;;; it is cut short, the last row of its budget becomes a hint saying how many lines
;;; are hidden, so a collapsed block never grows past its maximum.

(define-module (promptui widgets truncated-block)
  #:use-module (promptui style)
  #:use-module (promptui rect)
  #:use-module (promptui buffer)
  #:use-module (promptui text)
  #:use-module (promptui theme)
  #:use-module (promptui padding)
  #:use-module (promptui widgets common)
  #:use-module (srfi srfi-9)
  #:export (make-truncated-block
            truncated-block?
            truncated-block-content
            truncated-block-set-content!
            truncated-block-expanded?
            truncated-block-set-expanded!
            truncated-block-max-lines
            truncated-block-set-max-lines!
            truncated-block-set-padding!
            truncated-block-truncated?
            truncated-block-hidden-lines
            truncated-block-required-height
            truncated-block-render!))

(define %overflow-hint "▸")    ; ▸

(define-record-type <truncated-block>
  (%make-truncated-block theme padding content max-lines expanded hint?)
  truncated-block?
  (theme     tb-theme)
  (padding   tb-padding truncated-block-set-padding!)
  (content   truncated-block-content truncated-block-set-content!)
  ;; #f shows everything.
  (max-lines truncated-block-max-lines truncated-block-set-max-lines!)
  (expanded  truncated-block-expanded? truncated-block-set-expanded!)
  (hint?     tb-hint?))

(define* (make-truncated-block theme content #:key
                               (max-lines 5)
                               (expanded #f)
                               (show-overflow-hint #t)
                               (padding (padding-uniform 0)))
  "CONTENT is anything ->text accepts."
  (%make-truncated-block theme padding content max-lines expanded show-overflow-hint))

(define (h-overhead tb)
  (+ %border-width (padding-horizontal (tb-padding tb))))

(define (full-height tb text-width)
  (if (zero? text-width)
      0
      (text-height (truncated-block-content tb) text-width)))

(define (limit tb)
  "The row budget in force, or #f when the block shows everything."
  (and (not (truncated-block-expanded? tb)) (truncated-block-max-lines tb)))

(define (visible-lines tb text-width)
  (let ((full (full-height tb text-width))
        (budget (limit tb)))
    (cond
     ((not budget) full)
     ((<= full budget) full)
     ;; The hint takes a row from the budget.
     ((tb-hint? tb) (- budget 1))
     (else budget))))

(define (truncated-block-truncated? tb width)
  (let ((budget (limit tb)))
    (and budget (> (full-height tb (max0 (- width (h-overhead tb)))) budget))))

(define (max0 n) (if (< n 0) 0 n))

(define (truncated-block-hidden-lines tb width)
  (let ((text-width (max0 (- width (h-overhead tb)))))
    (max0 (- (full-height tb text-width) (visible-lines tb text-width)))))

(define (truncated-block-required-height tb width)
  "Rows the block needs at WIDTH, padding and any hint included."
  (let ((text-width (max0 (- width (h-overhead tb)))))
    (if (zero? text-width)
        0
        (let ((full (full-height tb text-width))
              (budget (limit tb)))
          (+ (if budget (min full budget) full)
             (padding-vertical (tb-padding tb)))))))

(define (truncated-block-render! tb area buf)
  (unless (rect-empty? area)
    (let* ((theme (tb-theme tb))
           (pad (tb-padding tb))
           (bg-style (make-style #:bg (theme-panel-bg theme)))
           (text-x (+ (rect-x area) %border-width (padding-left pad)))
           (text-width (max0 (- (rect-width area) (h-overhead tb)))))
      (fill-background! buf area bg-style)
      (draw-left-bar! buf area (theme-fg-style theme (theme-secondary theme)))
      (unless (zero? text-width)
        (let* ((full (full-height tb text-width))
               (budget (limit tb))
               (truncated? (and budget (> full budget)))
               (shown (visible-lines tb text-width))
               (content-area (make-rect text-x (+ (rect-y area) (padding-top pad))
                                        text-width
                                        (min shown (max0 (- (rect-height area)
                                                            (padding-vertical pad)))))))
          (unless (rect-empty? content-area)
            (render-paragraph! buf content-area (truncated-block-content tb)
                               #:style (theme-text-style theme)))
          (when (and truncated? (tb-hint? tb))
            (let ((hint-y (rect-bottom content-area)))
              (when (< hint-y (rect-bottom area))
                (draw-text! buf text-x hint-y (rect-right area)
                            (format #f "~a ~a more lines" %overflow-hint (- full shown))
                            (theme-muted-style theme)))))))))
  buf)
