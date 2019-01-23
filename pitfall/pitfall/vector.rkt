#lang racket/base
(require
  "core.rkt"
  "page.rkt"
  "color.rkt"
  racket/match
  racket/string
  sugar/unstable/js
  sugar/unstable/dict
  brag/support
  sugar/list
  racket/list)
(provide (all-defined-out))

(define default-ctm-value '(1 0 0 1 0 0))

(define (save doc)
  (set-pdf-ctm-stack! doc (cons (pdf-ctm doc) (pdf-ctm-stack doc)))
  (add-content doc "q"))

(define (restore doc)
  (set-pdf-ctm! doc
                 (if (pair? (pdf-ctm-stack doc))
                     (begin0
                       (car (pdf-ctm-stack doc))
                       (set-pdf-ctm-stack! doc (cdr (pdf-ctm-stack doc))))
                     default-ctm-value))
  (add-content doc "Q"))

(define (bezier-curve-to doc cp1x cp1y cp2x cp2y x y)
  (add-content doc (format "~a c" (string-join (map numberizer (list cp1x cp1y cp2x cp2y x y)) " "))))

(define (circle doc x y radius)
  (ellipse doc x y radius))

(define (clip doc [rule #f])
  (add-content doc (string-append "W" (winding-rule rule) " n")))

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

(define (path doc path-data)
  (parse-svg-path doc path-data)
  doc)

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

(define (rect doc x y w [h w])
  (add-content doc (format "~a re" (string-join (map numberizer (list x y w h)) " "))))

(define (rect-centered doc x y w [h w])
  (rect doc (- x (/ w 2)) (- y (/ h 2)) w h))

(define scale
  (match-lambda*
    [(list (? pdf? doc) (? number? x-factor)) (scale doc x-factor (mhash))]
    [(list (? pdf? doc) (? number? xFactor) (? hash? options)) (scale doc xFactor xFactor options)]
    [(list (? pdf? doc) (? number? xFactor) (? number? yFactor)) (scale doc xFactor yFactor (mhash))]
    [(list (? pdf? doc) (? number? xFactor) (? number? yFactor) (? hash? options))
     (match-define (list x y)
       (match-let ([(list xo yo) (hash-ref options 'origin '(0 0))])
         (list (* xo (- 1 xFactor)) (* yo (- 1 yFactor)))))
     (transform doc xFactor 0 0 yFactor x y)]))

(define (shear doc x y)
  (transform doc 1 y x 1 0 0))

(define (stroke doc [color #f] [width #f])
  (when width (line-width doc width))
  (when color (stroke-color doc color))
  (add-content doc "S"))

(define (transform doc scaleX shearY shearX scaleY mdx mdy)
  (define new-ctm (list scaleX shearY shearX scaleY mdx mdy))
  (set-pdf-ctm! doc (combine-transforms (pdf-ctm doc) new-ctm))
  (add-content doc (make-transform-string new-ctm)))

(define (translate doc x y)
  (transform doc 1 0 0 1 x y))

(define (winding-rule rule)
  (if (and (string? rule) (regexp-match #rx"^even-?odd$" rule)) "*" ""))

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

(define (parse-svg-path doc path)
  (define commands (parse path))
  (apply-commands commands doc))

(define (parse path)
  (define lex-1
    (lexer
     [(eof) eof]
     [alphabetic (string->symbol lexeme)]
     [(:: (:? "-") (:* numeric) (:? ".") (:+ numeric)) (string->number lexeme)]
     [(:or whitespace ",") (lex-1 input-port)]))
  (slicef-at (for/list ([tok (in-port lex-1 (open-input-string path))])
               tok) symbol?))

(module+ test
  (require rackunit)
  (check-equal?
   (parse "M 0,20 L 100,160 Q 130,200 150,120 C 190,-40 200,200 300,150 L 400,90")
   '((M 0 20)
     (L 100 160)
     (Q 130 200 150 120)
     (C 190 -40 200 200 300 150)
     (L 400 90)))

  (check-equal?
   (parse "M-122.304 84.285C-122.304 84.285 -122.203 86.179 -123.027 86.16C-123.851 86.141 -140.305 38.066 -160.833 40.309C-160.833 40.309 -143.05 32.956 -122.304 84.285z")
   '((M -122.304 84.285)
     (C -122.304 84.285 -122.203 86.179 -123.027 86.16)
     (C -123.851 86.141 -140.305 38.066 -160.833 40.309)
     (C -160.833 40.309 -143.05 32.956 -122.304 84.285)
     (z)))

  (check-equal? (parse "L100-160") '((L 100 -160))))

(define (apply-commands commands doc)
  (for/fold ([cx 0][cy 0][px 0][py 0][sx 0][sy 0])
            ([cmd (in-list commands)])
    (match-define (cons cmd-name cmd-args) cmd)
    (let loop ([cmd-name cmd-name][cmd-args cmd-args])
      (match-define (list a0 a1 a2 a3 a4 a5)
        (append cmd-args (make-list (- 6 (length cmd-args)) #f)))
      (case cmd-name
        [(M) (apply move-to doc cmd-args)
             (values a0 a1 #f #f a0 a1)]
        [(m) (loop 'M (list (+ cx a0) (+ cy a1)))]
        [(C) (apply bezier-curve-to doc cmd-args)
             (values a4 a5 a2 a3 sx sy)]
        [(c) (loop 'C (list (+ cx a0) (+ cy a1)
                            (+ cx a2) (+ cy a3)
                            (+ cx a4) (+ cy a5)))]
        [(S) (match-let ([(list px py) (if (not px)
                                           (list cx cy)
                                           (list px py))])
               (apply bezier-curve-to doc (- cx (- px cx)) (- cy (- py cy)) a0 a1 a2 a3)
               (values a2 a3 a0 a1 sx sy))]
        [(s) (loop 'S (list (+ cx a0) (+ cy a1)
                            (+ cx a2) (+ cy a3)))]
        [(L) (apply line-to doc cmd-args)
             (values a0 a1 #f #f sx sy)]
        [(l) (loop 'L (list (+ cx a0) (+ cy a1)))]
        [(H) (loop 'L (list a0 cy))]
        [(h) (loop 'L (list (+ cx a0) cy))]
        [(V) (loop 'L (list cx a0))]
        [(v) (loop 'L (list cx (+ cy a0)))]
        [(Q) (apply quadratic-curve-to doc cmd-args)
             (values a2 a3 a0 a1 sx sy)]
        [(q) (loop 'Q (list (+ cx a0) (+ cy a1)
                            (+ cx a2) (+ cy a3)))]
        [(T) (match-define (list px py)
               (if (not px)
                   (list cx py)
                   (list (- cx (- px cx) (- cy (- py cy))))))
             (apply quadratic-curve-to doc cmd-args)]
        ;; todo other path ops
        [(z) (apply close-path doc cmd-args)
             (values sx sy px py sx sy)]
        [else (raise-argument-error 'apply-commands "valid command name" cmd-name)]))))