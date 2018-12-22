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

(define default-ctm-value '(1 0 0 1 0 0))

(define (vector-mixin [% mixin-tester%])
  (class %
    (super-new)
    (field [@ctm default-ctm-value]
           [@ctm-stack null])

    (define/public (save)
      (set! @ctm-stack (cons @ctm @ctm-stack))
      (send this addContent "q"))

    (define/public (restore)
      (set! @ctm (if (pair? @ctm-stack)
                     (begin0
                       (car @ctm-stack)
                       (set! @ctm-stack (cdr @ctm-stack)))
                     default-ctm-value))
      (send this addContent "Q"))

    (define/public (close-path)
      (send this addContent "h"))

    (define/public (line-cap [c #f])
      (define cap-styles (hasheq 'butt 0 'round 1 'square 2))
      (send this addContent
            (format "~a J" (if (symbol? c)
                               (hash-ref cap-styles c)
                               ""))))

    (define/public (line-join [j #f])
      (define cap-styles (hasheq 'miter 0 'round 1 'bevel 2))
      (send this addContent
            (format "~a j" (if (symbol? j)
                               (hash-ref cap-styles j)
                               ""))))

    (define/public (line-width w)
      (send this addContent (format "~a w" (number w))))

    (define/public (dash length [options (mhash)])
      (cond
        [(list? length)
         (send this addContent
               (format "[~a] ~a d"
                       (string-join (map number length) " ")
                       (hash-ref options 'phase 0)))]
        [length
         (define space (hash-ref options 'space length))
         (define phase (hash-ref options 'phase 0))
         (send this addContent (format "[~a ~a] ~a d" (number length) (number space) (number phase)))] 
        [else this]))

    (define/public (move-to x y)
      (send this addContent (format "~a ~a m" x y)))

    (define/public (line-to x y)
      (send this addContent (format "~a ~a l" x y)))

    (define/public (bezier-curve-to cp1x cp1y cp2x cp2y x y)
      (send this addContent (format "~a c" (string-join (map number (list cp1x cp1y cp2x cp2y x y)) " "))))

    (define/public (quadratic-curve-to cpx cpy x y)
      (send this addContent (format "~a v" (string-join (map number (list cpx cpy x y)) " "))))
  
    (define/public (rect x y w h)
      (send this addContent (format "~a re" (string-join (map number (list x y w h)) " "))))

    (define/public (ellipse x y r1 [r2 r1])
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
      (move-to x ym)
      (bezier-curve-to x (- ym oy) (- xm ox) y xm y)
      (bezier-curve-to (+ xm ox) y xe (- ym oy) xe ym)
      (bezier-curve-to xe (+ ym oy) (+ xm ox) ye xm ye)
      (bezier-curve-to (- xm ox) ye x (+ ym oy) x ym)
      (close-path))

    (define/public (circle x y radius)
      (ellipse x y radius))

    (define/public (polygon . points)
      (match points
        [(cons (list x y) other-points)
         (move-to x y)
         (for ([pt (in-list other-points)])
           (match pt
             [(list x y)
              (line-to x y)]))
         (close-path)]
        [else this]))

    (define/public (path path-data)
      (parse-svg-path this path-data)
      this)

    (define/public (_windingRule rule)
      (if (and (string? rule) (regexp-match #rx"^even-?odd$" rule)) "*" ""))

    (define/public (fill [color #f] #:rule [rule #f])
      (when color (send this fill-color color)) ;; fill-color method is from color mixin
      (send this addContent (format "f~a" (_windingRule rule))))

    (define/public (stroke [color #f])
      (when color (send this stroke-color color))
      (send this addContent "S"))

    (define/public (fill-and-stroke [fill #f] [stroke fill] #:rule [rule #f])
      (when fill (send* this [fill-color fill] [stroke-color stroke]))
      (send this addContent (format "B~a" (_windingRule rule))))

    (define tm/c (list/c number? number? number? number? number? number?))
    (define/public (make-transform-string ctm)
      (format "~a cm" (string-join (map number ctm) " ")))

    (define/public (clip [rule #f])
      (send this addContent (string-append "W" (_windingRule rule) " n")))

    (define/public (transform scaleX shearY shearX scaleY mdx mdy)
      (define new-ctm (list scaleX shearY shearX scaleY mdx mdy))
      (set! @ctm (combine-transforms (· this @ctm) new-ctm))
      (send this addContent (make-transform-string new-ctm)))

    (define/public (shear x y)
      (transform 1 y x 1 0 0))

    (define/public (translate x y)
      (transform 1 0 0 1 x y))

    (define/public scale
      (match-lambda*
        [(list (? object? this) (? number? x-factor)) (scale x-factor (mhash))]
        [(list (? object? this) (? number? xFactor) (? hash? options)) (scale xFactor xFactor options)]
        [(list (? object? this) (? number? xFactor) (? number? yFactor)) (scale this xFactor yFactor (mhash))]
        [(list (? object? this) (? number? xFactor) (? number? yFactor) (? hash? options))
         (match-define (list x y)
           (match-let ([(list xo yo) (hash-ref options 'origin '(0 0))])
             (list (* xo (- 1 xFactor)) (* yo (- 1 yFactor)))))
         (transform xFactor 0 0 yFactor x y)]))))

(define (combine-transforms m new-ctm)
  (match-define (list m0 m1 m2 m3 m4 m5) m)
  (match-define (list m11 m12 m21 m22 dx dy) new-ctm)
  (list (+ (* m0 m11) (* m2 m12))
        (+ (* m1 m11) (* m3 m12))
        (+ (* m0 m21) (* m2 m22))
        (+ (* m1 m21) (* m3 m22))
        (+ (* m0 dx) (* m2 dy) m4)
        (+ (* m1 dx) (* m3 dy) m5)))

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
                '(1 0 0 1 50 -50.0)))