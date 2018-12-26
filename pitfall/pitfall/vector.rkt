#lang racket/base
(require
  "core.rkt"
  "page.rkt"
  "color.rkt"
  racket/class
  racket/match
  racket/string
  racket/contract
  sugar/unstable/class
  sugar/unstable/js
  sugar/unstable/dict
  "path.rkt")
(provide (all-defined-out))

(define default-ctm-value '(1 0 0 1 0 0))

(define (save doc)
  (set-$doc-ctm-stack! doc (cons ($doc-ctm doc) ($doc-ctm-stack doc)))
  (add-content doc "q"))

(define (restore doc)
  (set-$doc-ctm! doc
                 (if (pair? ($doc-ctm-stack doc))
                     (begin0
                       (car ($doc-ctm-stack doc))
                       (set-$doc-ctm-stack! doc (cdr ($doc-ctm-stack doc))))
                     default-ctm-value))
  (add-content doc "Q"))

(define (bezier-curve-to doc cp1x cp1y cp2x cp2y x y)
  (add-content doc (format "~a c" (string-join (map numberizer (list cp1x cp1y cp2x cp2y x y)) " "))))

(define (circle doc x y radius)
  (ellipse doc x y radius))

(define (close-path doc)
  (add-content doc "h"))

(define (dash doc length [options (mhash)])
  (cond
    [(list? length)
     (add-content doc
                  (format "[~a] ~a d"
                          (string-join (map numberizer length) " ")
                          (hash-ref options 'phase 0)))]
    [length
     (define space (hash-ref options 'space length))
     (define phase (hash-ref options 'phase 0))
     (add-content doc (format "[~a ~a] ~a d" (numberizer length) (numberizer space) (numberizer phase)))] 
    [else doc]))

(define (ellipse doc x y r1 [r2 r1])
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
  (move-to doc x ym)
  (bezier-curve-to doc x (- ym oy) (- xm ox) y xm y)
  (bezier-curve-to doc (+ xm ox) y xe (- ym oy) xe ym)
  (bezier-curve-to doc xe (+ ym oy) (+ xm ox) ye xm ye)
  (bezier-curve-to doc (- xm ox) ye x (+ ym oy) x ym)
  (close-path doc))

(define (fill doc [color #f] #:rule [rule #f])
  (when color (fill-color doc color)) ;; fill-color method is from color mixin
  (add-content doc (format "f~a" (winding-rule rule))))

(define (fill-and-stroke doc [fill #f] [stroke fill] #:rule [rule #f])
  (when fill (fill-color doc fill) (stroke-color doc stroke))
  (add-content doc (format "B~a" (winding-rule rule))))

(define (line-cap doc [c #f])
  (define cap-styles (hasheq 'butt 0 'round 1 'square 2))
  (add-content doc
               (format "~a J" (if (symbol? c)
                                  (hash-ref cap-styles c)
                                  ""))))

(define (line-join doc [j #f])
  (define cap-styles (hasheq 'miter 0 'round 1 'bevel 2))
  (add-content doc
               (format "~a j" (if (symbol? j)
                                  (hash-ref cap-styles j)
                                  ""))))

(define (line-to doc x y)
  (add-content doc (format "~a ~a l" x y)))

(define (line-width doc w)
  (add-content doc (format "~a w" (numberizer w))))

(define (move-to doc x y)
  (add-content doc (format "~a ~a m" x y)))

(define (polygon doc . points)
  (match points
    [(cons (list x y) other-points)
     (move-to doc x y)
     (for ([pt (in-list other-points)])
       (match pt
         [(list x y)
          (line-to doc x y)]))
     (close-path doc)]
    [else doc]))

(define (quadratic-curve-to doc cpx cpy x y)
  (add-content doc (format "~a v" (string-join (map numberizer (list cpx cpy x y)) " "))))

(define (rect doc x y w h)
  (add-content doc (format "~a re" (string-join (map numberizer (list x y w h)) " "))))

(define (shear doc x y)
  (transform doc 1 y x 1 0 0))

(define (stroke doc [color #f])
  (when color (stroke-color doc color))
  (add-content doc "S"))

(define (transform doc scaleX shearY shearX scaleY mdx mdy)
  (define new-ctm (list scaleX shearY shearX scaleY mdx mdy))
  (set-$doc-ctm! doc (combine-transforms ($doc-ctm doc) new-ctm))
  (add-content doc (make-transform-string new-ctm)))

(define (translate doc x y)
  (transform doc 1 0 0 1 x y))

(define (winding-rule rule)
  (if (and (string? rule) (regexp-match #rx"^even-?odd$" rule)) "*" ""))


#;(define (vector-mixin [% mixin-tester%])
    (class %
      (super-new)
      (field [@ctm default-ctm-value]
             [@ctm-stack null])
      (inherit add-content) ; from base
      (inherit stroke-color fill-color) ; from color

      (define/public (save)
        (set! @ctm-stack (cons @ctm @ctm-stack))
        (add-content "q"))

      (define/public (restore)
        (set! @ctm (if (pair? @ctm-stack)
                       (begin0
                         (car @ctm-stack)
                         (set! @ctm-stack (cdr @ctm-stack)))
                       default-ctm-value))
        (add-content "Q"))

      (define/public (close-path)
        (add-content "h"))

      

      

      (define/public (move-to x y)
        (add-content (format "~a ~a m" x y)))

      (define/public (line-to x y)
        (add-content (format "~a ~a l" x y)))

      (define/public (bezier-curve-to cp1x cp1y cp2x cp2y x y)
        (add-content (format "~a c" (string-join (map numberizer (list cp1x cp1y cp2x cp2y x y)) " "))))

      (define/public (quadratic-curve-to cpx cpy x y)
        (add-content (format "~a v" (string-join (map numberizer (list cpx cpy x y)) " "))))
  
      

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

      

      (define/public (path path-data)
        (parse-svg-path this path-data)
        this)

      (define/public (_windingRule rule)
        (if (and (string? rule) (regexp-match #rx"^even-?odd$" rule)) "*" ""))

      (define/public (fill [color #f] #:rule [rule #f])
        (when color (fill-color color)) ;; fill-color method is from color mixin
        (add-content (format "f~a" (_windingRule rule))))

      (define/public (stroke [color #f])
        (when color (stroke-color color))
        (add-content "S"))

      

      (define tm/c (list/c number? number? number? number? number? number?))
      (define/public (make-transform-string ctm)
        (format "~a cm" (string-join (map numberizer ctm) " ")))

      (define/public (clip [rule #f])
        (add-content (string-append "W" (_windingRule rule) " n")))

      

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

(define (make-transform-string ctm)
  (format "~a cm" (string-join (map numberizer ctm) " ")))

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