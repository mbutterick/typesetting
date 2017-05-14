#lang racket/base
(require racket/class racket/match racket/string racket/format)
(provide vector-mixin)
(require "helper.rkt")

;; This constant is used to approximate a symmetrical arc using a cubic
;; Bezier curve.
(define KAPPA (* 4 (/ (- (sqrt 2) 1) 3.0)))

(define default-ctm-value '(1 0 0 1 0 0))

(define (vector-mixin %)
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
      (send this addContent (format "~a ~a ~a ~a ~a ~a c" cp1x cp1y cp2x cp2y x y)))


    (public [@ellipse ellipse])
    (define (@ellipse x y r1 [r2 r1])
      ;; based on http://stackoverflow.com/questions/2172798/how-to-draw-an-oval-in-html5-canvas/2173084#2173084
      (-= x r1)
      (-= y r2)
      (define ox (* r1 KAPPA))
      (define oy (* r2 KAPPA))
      (define xe (+ x (* r1 2)))
      (define ye (+ y (* r2 2)))
      (define xm (+ x r1))
      (define ym (+ y r2))
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
      (if (and (string? rule) (regexp-match #rx"^even-?odd$" rule))
          "*"
          ""))

    (define/public (fill color [rule #f])
      (when (regexp-match #rx"^(even-?odd)|(non-?zero)$" color)
        (set! rule color)
        (set! color #f))
      (when color (send this fillColor color)) ;; fillColor method is from color mixin
      (send this addContent (format "f~a" (@_windingRule rule))))


    (public [@transform transform])
    (define (@transform m11 m12 m21 m22 dx dy #:debug [debug #f])
      ;; keep track of the current transformation matrix
      (match-define (list m0 m1 m2 m3 m4 m5) @_ctm)
      (set! @_ctm (list (+ (* m0 m11) (* m2 m12))
                        (+ (* m1 m11) (* m3 m12))
                        (+ (* m0 m21) (* m2 m22))
                        (+ (* m1 m21) (* m3 m22))
                        (+ (* m0 dx) (* m2 dy) m4)
                        (+ (* m1 dx) (* m3 dy) m5)))
      (define values (string-join (map number (list m11 m12 m21 m22 dx dy)) " "))
      (define result (format "~a cm" values))
      (if debug
          result
          (send this addContent result)))


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
         (define x 0) (define y 0)
         (when (hash-ref options 'origin #f)
           (match-define (list x y) (hash-ref options 'origin))
           (-= x (* x xFactor))
           (-= y (* y yFactor)))
         (@transform xFactor 0 0 yFactor x y)]))
      

    ))

(module+ test
  (require rackunit)
  (define v (new (vector-mixin object%)))
  (check-equal? (· v _ctm) default-ctm-value)
  (check-equal? (send v transform 1 2 3 4 5 6 #:debug #t) "1 2 3 4 5 6 cm")
  (check-equal? (· v _ctm) '(1 2 3 4 5 6))
  (check-equal? (send v transform 1 2 3 4 5 6 #:debug #t) "1 2 3 4 5 6 cm")
  (check-equal? (· v _ctm) '(7 10 15 22 28 40))
  (check-equal? (send v transform 1 2 3 4 5 6 #:debug #t) "1 2 3 4 5 6 cm")
  (check-equal? (· v _ctm) '(37 54 81 118 153 222)))