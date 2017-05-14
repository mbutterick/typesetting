#lang racket/base
(require racket/class racket/match racket/string racket/format racket/contract)
(require "helper.rkt" "params.rkt")
(provide (contract-out
          [vector-mixin (class? . -> .
                                (class/c [initVector (->m void?)]
                                         [save (->m object?)]
                                         [restore (->m object?)]
                                         [closePath (->m object?)]
                                         [moveTo (number? number? . ->m . object?)]
                                         [lineTo (number? number? . ->m . object?)]
                                         [bezierCurveTo ( number? number? number? number? number? number? . ->m . object?)]
                                         [ellipse ((number? number? number?) (number?) . ->*m . object?)]
                                         [circle (number? number? number? . ->m . object?)]
                                         [_windingRule (string? . ->m . string?)]
                                         [fill ((string?) ((or/c string? #f)) . ->*m . object?)]
                                         [transform (number? number? number? number? number? number? . ->m . object?)]
                                         [translate (number? number? . ->m . object?)]
                                         [scale (case->m
                                                 (number? . -> . object?)
                                                 (number? hash? . -> . object?)
                                                 (number? number? . -> . object?)
                                                 (number? number? hash? . -> . object?))]))]))


;; This constant is used to approximate a symmetrical arc using a cubic
;; Bezier curve.
(define kappa (* 4 (/ (- (sqrt 2) 1) 3.0)))

(define default-ctm-value '(1 0 0 1 0 0))

(define (make-transform-string ctm)
  (format "~a cm" (string-join (map number ctm) " ")))

(define (combine-transforms n-transform m-transform)
  (match-define (list n11 n12 n21 n22 ndx ndy) n-transform)
  (match-define (list m11 m12 m21 m22 mdx mdy) m-transform)
  (list (+ (* n11 m11) (* n21 m12))
        (+ (* n12 m11) (* n22 m12))
        (+ (* n11 m21) (* n21 m22))
        (+ (* n12 m21) (* n22 m22))
        (+ (* n11 mdx) (* n21 mdy) ndx)
        (+ (* n12 mdx) (* n22 mdy) ndy)))

(define (vector-mixin [% mixin-tester%])
  (class %
    (super-new)
    (field [(@_ctm _ctm) default-ctm-value]
           [(@_ctmStack _ctmStack) null])
    
    (define/public (initVector)
      (set! @_ctm default-ctm-value)
      (set! @_ctmStack null))

    (define/public (save)
      (push! @_ctmStack @_ctm)
      (send this addContent "q"))

    (define/public (restore)
      (set! @_ctm (if (pair? @_ctmStack) (pop! @_ctmStack) default-ctm-value))
      (send this addContent "Q"))

    (public [@closePath closePath])
    (define (@closePath)
      (send this addContent "h"))      

    (public [@moveTo moveTo])
    (define (@moveTo x y)
      (send this addContent (format "~a ~a m" x y)))

    (public [@lineTo lineTo])
    (define (@lineTo x y)
      (send this addContent (format "~a ~a l" x y)))


    (public [@bezierCurveTo bezierCurveTo])
    (define (@bezierCurveTo cp1x cp1y cp2x cp2y x y)
      (send this addContent (format "~a c" (string-join (map number (list cp1x cp1y cp2x cp2y x y)) " "))))


    (public [@ellipse ellipse])
    (define (@ellipse x y r1 [r2 r1])
      ;; based on http://stackoverflow.com/questions/2172798/how-to-draw-an-oval-in-html5-canvas/2173084#2173084
      (-= x r1)
      (-= y r2)
      (define ox (* r1 kappa)) ; control point offset horizontal
      (define oy (* r2 kappa)) ; control point offset vertical
      (define xe (+ x (* r1 2))) ; x-end
      (define ye (+ y (* r2 2))) ; y-end
      (define xm (+ x r1)) ; x-middle
      (define ym (+ y r2)) ; y-middle
      (@moveTo x ym)
      (@bezierCurveTo x (- ym oy) (- xm ox) y xm y)
      (@bezierCurveTo (+ xm ox) y xe (- ym oy) xe ym)
      (@bezierCurveTo xe (+ ym oy) (+ xm ox) ye xm ye)
      (@bezierCurveTo (- xm ox) ye x (+ ym oy) x ym)
      (@closePath))
      
    
    (public [@circle circle])
    (define (@circle x y radius)
      (@ellipse x y radius))


    (public [@path path])
    (define (@path path)
      ;; SVGPath.apply this, path ; todo
      this)

    (public [@_windingRule _windingRule])
    (define (@_windingRule rule)
      (if (and (string? rule) (regexp-match #rx"^even-?odd$" rule)) "*" ""))

    (define/public (fill color [rule #f])
      (when (regexp-match #rx"^(even-?odd)|(non-?zero)$" color)
        (set! rule color)
        (set! color #f))
      (when color (send this fillColor color)) ;; fillColor method is from color mixin
      (send this addContent (format "f~a" (@_windingRule rule))))
      
    (public [@transform transform])
    (define (@transform . new-ctm)
      ;; keep track of the current transformation matrix
      (set! @_ctm (combine-transforms @_ctm new-ctm))
      (send this addContent (make-transform-string new-ctm)))


    (public [@translate translate])
    (define (@translate x y)
      (@transform 1 0 0 1 x y))

    (public [@scale scale])
    (define @scale
      (match-lambda*
        [(list (? number? xFactor)) (@scale xFactor (mhash))]
        [(list (? number? xFactor) (? hash? options)) (@scale xFactor xFactor options)]
        [(list (? number? xFactor) (? number? yFactor)) (@scale xFactor yFactor (mhash))]
        [(list (? number? xFactor) (? number? yFactor) (? hash? options))
         (match-define (list x y)
           (match-let ([(list xo yo) (hash-ref options 'origin '(0 0))])
             (list (* xo (- 1 xFactor)) (* yo (- 1 yFactor)))))
         (@transform xFactor 0 0 yFactor x y)]))
     
    ))

(module+ test
  (require rackunit)
  (define ctm default-ctm-value)
  (define ctm2 '(1 2 3 4 5 6))
  (set! ctm (combine-transforms ctm ctm2))
  (check-equal? ctm '(1 2 3 4 5 6))
  (set! ctm (combine-transforms ctm ctm2))
  (check-equal? ctm '(7 10 15 22 28 40))
  (set! ctm (combine-transforms ctm ctm2))
  (check-equal? ctm '(37 54 81 118 153 222)))