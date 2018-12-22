#lang racket/base
(require
  "helper.rkt"
  racket/class
  racket/match
  racket/string
  racket/contract
  sugar/unstable/class
  sugar/unstable/js
  sugar/unstable/dict
  "path.rkt")
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
     lineCap
     lineJoin
     lineWidth
     dash
     moveTo
     lineTo
     bezierCurveTo
     quadraticCurveTo
     rect
     ellipse
     circle
     polygon
     path
     _windingRule
     fill
     stroke
     fillAndStroke
     clip
     shear
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

(define/contract (lineCap this [c #f])
  ((or/c 'butt 'round 'square #f) . ->m . object?)
  (define cap-styles (hasheq 'butt 0 'round 1 'square 2))
  (send this addContent
        (format "~a J" (if (symbol? c)
                           (hash-ref cap-styles c)
                           ""))))


(define/contract (lineJoin this [j #f])
  ((or/c 'miter 'round 'bevel #f) . ->m . object?)
  (define cap-styles (hasheq 'miter 0 'round 1 'bevel 2))
  (send this addContent
        (format "~a j" (if (symbol? j)
                           (hash-ref cap-styles j)
                           ""))))


(define/contract (lineWidth this w)
  (number? . ->m . object?)
  (send this addContent (format "~a w" (number w))))


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
  

(define/contract (rect this x y w h)
  (number? number? number? number? . ->m . object?)
  (send this addContent (format "~a re" (string-join (map number (list x y w h)) " "))))

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


(define/contract (fill this [color #f] #:rule [rule #f])
  (() ((or/c color-string? #f) #:rule (or/c string? #f)) . ->*m . object?)
  (when color (send this fill-color color)) ;; fill-color method is from color mixin
  (send this addContent (format "f~a" (_windingRule rule))))


(define/contract (stroke this [color #f])
  (() ((or/c color-string? #f)) . ->*m . object?)
  (when color (send this stroke-color color))
  (send this addContent "S"))


(define/contract (fillAndStroke this [fill #f] [stroke fill] #:rule [rule #f])
  (() ((or/c color-string? #f) (or/c color-string? #f) #:rule (or/c string? #f)) . ->*m . object?)
  (when fill (send* this [fill-color fill] [stroke-color stroke]))
  (send this addContent (format "B~a" (_windingRule rule))))


(define tm/c (list/c number? number? number? number? number? number?))
(define/contract (make-transform-string ctm)
  (tm/c . -> . string?)
  (format "~a cm" (string-join (map number ctm) " ")))


(define/contract (combine-transforms m new-ctm)
  (tm/c tm/c . -> . tm/c)
  (match-define (list m0 m1 m2 m3 m4 m5) m)
  (match-define (list m11 m12 m21 m22 dx dy) new-ctm)
  (list (+ (* m0 m11) (* m2 m12))
        (+ (* m1 m11) (* m3 m12))
        (+ (* m0 m21) (* m2 m22))
        (+ (* m1 m21) (* m3 m22))
        (+ (* m0 dx) (* m2 dy) m4)
        (+ (* m1 dx) (* m3 dy) m5)))


(define/contract (clip this [rule #f])
  (() ((or/c string? #f)) . ->*m . object?)
  (send this addContent (string-append "W" (_windingRule rule) " n")))


(define/contract (transform this scaleX shearY shearX scaleY mdx mdy)
  (number? number? number? number? number? number? . ->m . object?)
  (define new-ctm (list scaleX shearY shearX scaleY mdx mdy))
  (set-field! _ctm this (combine-transforms (· this _ctm) new-ctm))
  (send this addContent (make-transform-string new-ctm)))


(define/contract (shear this x y)
  (number? number? . ->m . object?)
  (transform this 1 y x 1 0 0))


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
  (check-equal? ctm '(37 54 81 118 153 222))

  (check-equal? (combine-transforms '(1 0 0 -1 0 792.0) '(1 0 0 1 50 50))
                '(1 0 0 -1 50 742.0))

  (check-equal? (combine-transforms '(1 0 0 -1 50 742.0) '(1 0 0 -1 0 792))
                '(1 0 0 1 50 -50.0))
  )