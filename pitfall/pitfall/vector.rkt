#lang pitfall/racket
(require "path.rkt")
(provide vector-mixin default-ctm-value)

(define (vector-mixin [% mixin-tester%])
  (class %
    (super-new)
    (field [_ctm default-ctm-value]
           [_ctmStack null])
    (as-methods
     initVector
     save
     restore
     closePath
     dash
     moveTo
     lineTo
     bezierCurveTo
     quadraticCurveTo
     ellipse
     circle
     polygon
     path
     _windingRule
     fill
     stroke
     transform
     translate
     scale)))


(define default-ctm-value '(1 0 0 1 0 0))


(define/contract (initVector this)
  (->m void?)
  (set-field! _ctm this default-ctm-value)
  (set-field! _ctmStack this null))


(define/contract (save this)
  (->m object?)
  (push-field! _ctmStack this (· this _ctm))
  (send this addContent "q"))


(define/contract (restore this)
  (->m object?)
  (set-field! _ctm this (if (pair? (· this _ctmStack))
                            (pop-field! _ctmStack this)
                            default-ctm-value))
  (send this addContent "Q"))


(define/contract (closePath this)
  (->m object?)
  (send this addContent "h"))


(define/contract (dash this length [options (mhash)])
  (((or/c number? (listof number?) #f)) (hash?) . ->*m . object?)
  (cond
    [length
     (cond
       [(list? length)
        (send this addContent
              (format "[~a] ~a d"
                      (string-join (map number length) " ")
                      (hash-ref options 'phase 0)))]
       [else
        (define space (hash-ref options 'space length))
        (define phase (hash-ref options 'phase 0))
        (send this addContent (format "[~a ~a] ~a d" (number length) (number space) (number phase)))])] 
    [else this]))


(define/contract (moveTo this x y)
  (number? number? . ->m . object?)
  (send this addContent (format "~a ~a m" x y)))


(define/contract (lineTo this x y)
  (number? number? . ->m . object?)
  (send this addContent (format "~a ~a l" x y)))


(define/contract (bezierCurveTo this cp1x cp1y cp2x cp2y x y)
  (number? number? number? number? number? number? . ->m . object?)
  (send this addContent (format "~a c" (string-join (map number (list cp1x cp1y cp2x cp2y x y)) " "))))


(define/contract (quadraticCurveTo this cpx cpy x y)
  (number? number? number? number . ->m . object?)
  (send this addContent (format "~a v" (string-join (map number (list cpx cpy x y)) " "))))
  

(define/contract (ellipse this x y r1 [r2 r1])
  ((number? number? number?) (number?) . ->*m . object?)
  ;; based on http://stackoverflow.com/questions/2172798/how-to-draw-an-oval-in-html5-canvas/2173084#2173084
  ;; This constant is used to approximate a symmetrical arc using a cubic Bezier curve.
  (define kappa (* 4 (/ (- (sqrt 2) 1) 3.0)))
  (-= x r1)
  (-= y r2)
  (define ox (* r1 kappa)) ; control point offset horizontal
  (define oy (* r2 kappa)) ; control point offset vertical
  (define xe (+ x (* r1 2))) ; x-end
  (define ye (+ y (* r2 2))) ; y-end
  (define xm (+ x r1)) ; x-middle
  (define ym (+ y r2)) ; y-middle
  (moveTo this x ym)
  (bezierCurveTo this x (- ym oy) (- xm ox) y xm y)
  (bezierCurveTo this (+ xm ox) y xe (- ym oy) xe ym)
  (bezierCurveTo this xe (+ ym oy) (+ xm ox) ye xm ye)
  (bezierCurveTo this (- xm ox) ye x (+ ym oy) x ym)
  (closePath this))


(define/contract (circle this x y radius)
  (number? number? number? . ->m . object?)
  (ellipse this x y radius))


(define/contract (polygon this . points)
  (() () #:rest (listof (list/c number? number?)) . ->*m . object?)
  (cond
    [(pair? points)
     (apply moveTo this (car points))
     (for ([pt (in-list (cdr points))])
       (apply lineTo this pt))
     (closePath this)]
    [else this]))


(define/contract (path this path-data)
  (string? . ->m . object?)
  (parse-svg-path this path-data)
  this)


(define/contract (_windingRule rule)
  ((or/c string? #f) . -> . string?)
  (if (and (string? rule) (regexp-match #rx"^even-?odd$" rule)) "*" ""))


(define/contract (fill this color [rule #f])
  ((color-string?) ((or/c string? #f)) . ->*m . object?)
  (when (regexp-match #rx"^(even-?odd)|(non-?zero)$" color)
    (set! rule color)
    (set! color #f))
  (when color (send this fillColor color)) ;; fillColor method is from color mixin
  (send this addContent (format "f~a" (_windingRule rule))))


(define/contract (stroke this [color #f])
  (() ((or/c color-string? #f)) . ->*m . object?)
  (when color (send this strokeColor color))
  (send this addContent "S"))


(define tm/c (list/c number? number? number? number? number? number?))
(define/contract (make-transform-string ctm)
  (tm/c . -> . string?)
  (format "~a cm" (string-join (map number ctm) " ")))


(define/contract (combine-transforms m-transform n-transform)
  (tm/c tm/c . -> . tm/c)
  (match-define (list m11 m12 m21 m22 mdx mdy) m-transform)
  (match-define (list n11 n12 n21 n22 ndx ndy) n-transform)
  (list (+ (* n11 m11) (* n21 m12))
        (+ (* n12 m11) (* n22 m12))
        (+ (* n11 m21) (* n21 m22))
        (+ (* n12 m21) (* n22 m22))
        (+ (* n11 mdx) (* n21 mdy) ndx)
        (+ (* n12 mdx) (* n22 mdy) ndy)))


(define/contract (transform this m11 m12 m21 m22 mdx mdy)
  (number? number? number? number? number? number? . ->m . object?)
  (define new-ctm (list m11 m12 m21 m22 mdx mdy))
  (set-field! _ctm this (combine-transforms (· this _ctm) new-ctm))
  (send this addContent (make-transform-string new-ctm)))


(define/contract (translate this x y)
  (number? number? . ->m . object?)
  (transform this 1 0 0 1 x y))


(define/contract scale
  (case->m
   (number? . -> . object?)
   (number? hash? . -> . object?)
   (number? number? . -> . object?)
   (number? number? hash? . -> . object?))
  (match-lambda*
    [(list (? object? this) (? number? xFactor)) (scale xFactor (mhash))]
    [(list (? object? this) (? number? xFactor) (? hash? options)) (scale xFactor xFactor options)]
    [(list (? object? this) (? number? xFactor) (? number? yFactor)) (scale this xFactor yFactor (mhash))]
    [(list (? object? this) (? number? xFactor) (? number? yFactor) (? hash? options))
     (match-define (list x y)
       (match-let ([(list xo yo) (hash-ref options 'origin '(0 0))])
         (list (* xo (- 1 xFactor)) (* yo (- 1 yFactor)))))
     (transform this xFactor 0 0 yFactor x y)]))



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