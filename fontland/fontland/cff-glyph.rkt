#lang debug racket
(require racket/match
         xenomorph
         fontland/struct
         fontland/table-stream
         fontland/table/cff/cff-font
         fontland/path)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/glyph/CFFGlyph.js
|#

(define (bias this s)
  (cond
    [(< (length s) 1240) 107]
    [(< (length s) 33900) 1131]
    [else 32768]))

(define (getPath this)
  (define stream (ttf-font-port (glyph-font this)))
  ;;;(define pos (pos stream))

  (define cff (get-table (glyph-font this) 'CFF_))
  (define str (list-ref (hash-ref (hash-ref cff 'topDict) 'CharStrings) (glyph-id this)))
  (define end (+ (hash-ref str 'offset) (hash-ref str 'length)))
  (pos stream (hash-ref str 'offset))

  #R (pos stream)
  #R end
  
  (define path (Path))
  (define stack null)
  (define trans null)

  (define width #false)
  (define nStems 0)
  (define x 0)
  (define y 0)
  (define usedGsubrs (cff-glyph-_usedGsubrs this))
  (define usedSubrs (cff-glyph-_usedSubrs this))
  (define open #false)

  (define gsubrs (hash-ref cff 'globalSubrIndex null))
  (define gsubrsBias (bias this gsubrs))

  (define privateDict (privateDictForGlyph cff (glyph-id this)))
  (define subrs (hash-ref privateDict 'Subrs null))
  (define subrsBias (bias this subrs))

  ;; skip variations shit
  #;(define vstore (and (hash-ref* cff 'topDict 'vstore)
                        (hash-ref* cff 'topDict 'vstore)))
  #;(define vsindex (hash-ref privateDict 'vsindex))
  #;(define variationProcessor )

  (define-syntax-rule (shift ID)
    (begin0
      (car ID)
      (set! ID (cdr ID))))

  (define-syntax-rule (push ID VAL ...)
    (begin
      (set! ID (append ID (list VAL ...)))
      ID))

  (define-syntax-rule (pop ID)
    (cond
      [(> (length ID) 0)
       (define-values (head last) (split-at-right ID 1))
       (set! ID head)
       last]))

  (define (checkWidth)
    (unless width
      (set! width (+ (shift stack) (hash-ref privateDict 'nominalWidthX)))))

  (define (parseStems)
    (when (not (zero? (modulo (length stack) 2)))
      (checkWidth))

    (set! nStems (+ nStems (arithmetic-shift (length stack) -1)))
    (set! stack null)
    (length stack))

  (define (moveTo x y)
    (when open
      (path-closePath path))
    (path-moveTo path x y)
    (set! open #true))
                      
  (define (parse)
    #R 'in-parse
    (let/ec return
      (let loop ()
        (when  (< (pos stream) end)
          (define op (read-byte stream))
          #R op
          (cond
            [(< op 32)
             (case op
               [(1 ;; hstem
                 3 ;; vstem
                 18 ;; hstemhm
                 23) ;; vstemhm
                (parseStems)]
               [(4) ;; vmoveto
                (when (> (length stack) 1)
                  (checkWidth))
                (set! y (+ y (shift stack)))
                (moveTo x y)]
               [(5) ;; rlineto
                (let loop ()
                  (when (>= (length stack) 2)
                    (set! x (+ x (shift stack)))
                    (set! y (+ y (shift stack)))
                    (path-lineTo path x y)
                    (loop)))]
               [(6 ;; hlineto
                 7) ;; vlineto
                (define phase (= op 6))
                (let loop ()
                  (when (>= (length stack) 1)
                    (if phase
                        (set! x (+ x (shift stack)))
                        (set! y (+ y (shift stack)))))
                  (loop))
                (path-lineTo path x y)
                (set! phase (not phase))]
               [(8) ;; rrcurveto
                (let loop ()
                  (when (> (length stack) 0)
                    (define c1x (+ x (shift stack)))
                    (define c1y (+ y (shift stack)))
                    (define c2x (+ c1x (shift stack)))
                    (define c2y (+ c1y (shift stack)))
                    (path-bezierCurveTo path c1x c1y c2x c2y x y)))]

               [(10) ;; callsubr
                (define index (+ (pop stack) subrsBias))
                (define subr (list-ref subrs index))
                (when subr
                  (hash-set! usedSubrs index #true)
                  (define p (pos stream))
                  (define e end)
                  (pos stream (hash-ref subr 'offset))
                  (set! end (+ (hash-ref subr 'offset) (hash-ref subr 'length)))
                  (parse)
                  (pos stream p)
                  (set! end e))]
               [(11) ;; return
                (cond
                  [(>= (hash-ref cff 'version) 2) (void)]
                  [else (return)])]
               [(14) ;; endchar
                (cond
                  [(>= (hash-ref cff 'version) 2) (void)]
                  [else
                   (when (> (length stack) 0)
                     (checkWidth))
                   (when open
                     (path-closePath path)
                     (set! open #false))])]
               [(15) ;; vsindex
                (when (< (hash-ref cff 'version) 2)
                  (error 'vsindex-operator-not-supported))]

               [(16) ;; blend
                (error 'blend-operator-not-supported)]

               [(19 ;; hintmask
                 20) ;; cntrmask
                (parseStems)
                (pos stream (+ (pos stream) (arithmetic-shift (+ nStems 7) -3)))]

               [(21) ;; rmoveto
                (when (> (length stack) 2)
                  (checkWidth))

                (set! x (+ x (shift stack)))
                (set! y (+ y (shift stack)))
                (moveTo x y)]

               [(22) ;; hmoveto
                (when (> (length stack) 1)
                  (checkWidth))

                (set! x (+ x (shift stack)))
                (moveTo x y)]

               [(24) ;; rcurveline
                (let loop ()
                  (when (>= (length stack) 8)
                    (define c1x (+ x (shift stack)))
                    (define c1y (+ y (shift stack)))
                    (define c2x (+ c1x (shift stack)))
                    (define c2y (+ c1y (shift stack)))
                    (set! x (+ c2x (shift stack)))
                    (set! y (+ c2y (shift stack)))
                    (path-bezierCurveTo path c1x c1y c2x c2y x y)
                    (loop)))

                (set! x (+ x (shift stack)))
                (set! y (+ y (shift stack)))
                (path-lineTo path x y)]

               [(25) ;; rlinecurve
                (let loop ()
                  (when (>= (length stack) 8)
                    (set! x (+ x (shift stack)))
                    (set! y (+ y (shift stack)))
                    (path-lineTo path x y)
                    (loop)))

                (define c1x (+ x (shift stack)))
                (define c1y (+ y (shift stack)))
                (define c2x (+ c1x (shift stack)))
                (define c2y (+ c1y (shift stack)))
                (set! x (+ c2x (shift stack)))
                (set! y (+ c2y (shift stack)))
                (path-bezierCurveTo path c1x c1y c2x c2y x y)]

               [(26) ;; vvcurveto
                (when (not (zero? (modulo (length stack) 2)))
                  (set! x (+ x (shift stack))))

                (let loop ()
                  (when (>= (length stack) 4)
                    (define c1x x)
                    (define c1y (+ y (shift stack)))
                    (define c2x (+ c1x (shift stack)))
                    (define c2y (+ c1y (shift stack)))
                    (set! x c2x)
                    (set! y (+ c2y (shift stack)))
                    (path-bezierCurveTo path c1x c1y c2x c2y x y)
                    (loop)))]

               [(27) ;; hhcurveto
                (when (not (zero? (modulo (length stack) 2)))
                  (set! y (+ y (shift stack))))

                (let loop ()
                  (when (>= (length stack) 4)
                    (define c1x (+ x (shift stack)))
                    (define c1y y)
                    (define c2x (+ c1x (shift stack)))
                    (define c2y (+ c1y (shift stack)))
                    (set! x (+ c2x (shift stack)))
                    (set! y c2y)
                    (path-bezierCurveTo path c1x c1y c2x c2y x y)
                    (loop)))]

               [(28) ;; shortint
                (push stack (decode int16be stream))]

               [(29) ;; callgsubr
                (define index (+ (pop stack) gsubrsBias))
                (define subr (hash-ref gsubrs index))
                (when subr
                  (hash-set! usedGsubrs index #true)
                  (define p (pos stream))
                  (define e end)
                  (pos stream (hash-ref subr 'offset))
                  (set! end (+ (hash-ref subr 'offset) (hash-ref subr 'length)))
                  (parse)
                  (pos stream p)
                  (set! end e))]

               [(30 ;; vhcurveto
                 31) ;; hvcurveto
                (define phase (= op 31))
                (let loop ()
                  (when (>= (length stack) 4)
                    (cond
                      [phase
                       (define c1x (+ x (shift stack)))
                       (define c1y y)
                       (define c2x (+ c1x (shift stack)))
                       (define c2y (+ c1y (shift stack)))
                       (set! y (+ c2y (shift stack)))
                       (set! x (+ c2x (if (= (length stack) 1) (shift stack) 0)))
                       (path-bezierCurveTo path c1x c1y c2x c2y x y)
                       (set! phase (not phase))]
                      [else
                       (define c1x x)
                       (define c1y (+ y (shift stack)))
                       (define c2x (+ c1x (shift stack)))
                       (define c2y (+ c1y (shift stack)))
                       (set! x (+ c2x (shift stack)))
                       (set! y (+ c2y (if (= (length stack) 1) (shift stack) 0)))
                       (path-bezierCurveTo path c1x c1y c2x c2y x y)
                       (set! phase (not phase))])
                    (loop)))]

               [(12)
                (define op (read-byte stream))
                (println "warning: check truthiness")
                (case op
                  [(3) ;; and
                   (define a (pop stack))
                   (define b (pop stack))
                   (push stack (if (and a b) 1 0))]
                  [(4) ;; or
                   (define a (pop stack))
                   (define b (pop stack))
                   (push stack (if (or a b) 1 0))]

                  [(5) ;; not
                   (define a (pop stack))
                   (push stack (if a 0 1))]

                  [(9) ;; abs
                   (define a (pop stack))
                   (push stack (abs a))]

                  [(10) ;; add
                   (define a (pop stack))
                   (define b (pop stack))
                   (push stack (+ a b))]

                  [(11) ;; sub
                   (define a (pop stack))
                   (define b (pop stack))
                   (push stack (- a b))]

                  [(12) ;; div
                   (define a (pop stack))
                   (define b (pop stack))
                   (push stack (/ a b 1.0))]

                  [(14) ;; neg
                   (define a (pop stack))
                   (push stack (- a))]

                  [(15) ;; eq
                   (define a (pop stack))
                   (define b (pop stack))
                   (push stack (if (- a b) 1 0))]

                  [(18) ;; drop
                   (pop stack)]

                  [(20) ;; put
                   (define val (pop stack))
                   (define idx (pop stack))
                   (set! trans (list-set trans idx val))]

                  [(21) ;; get
                   (define idx (pop stack))
                   (push stack (or (list-ref trans idx) 0))]

                  [(22) ;; ifelse
                   (define s1 (pop stack))
                   (define s2 (pop stack))
                   (define v1 (pop stack))
                   (define v2 (pop stack))
                   (push stack (if (<= v1 v2) s1 s2))]

                  [(23) ;; random
                   (push stack (random))]

                  [(24) ;; mul
                   (define a (pop stack))
                   (define b (pop stack))
                   (push stack (* a b))]

                  [(26) ;; sqrt
                   (define a (pop stack))
                   (push stack (sqrt a))]

                  [(26) ;; dup
                   (define a (pop stack))
                   (push stack a a)]

                  [(28) ;; exch
                   (define a (pop stack))
                   (define b (pop stack))
                   (push stack b a)]

                  [(29) ;; index
                   (define idx (pop stack))
                   (cond
                     [(< idx 0)
                      (set! idx 0)]
                     [(> idx (- (length stack) 1))
                      (set! idx (- (length stack) 1))])

                   (push stack (list-ref stack idx))]

                  [(30) ;; roll
                   (define n (pop stack))
                   (define j (pop stack))

                   (cond
                     [(>= j 0)
                      (let loop ()
                        (when (> j 0)
                          (define t (list-ref stack (- n 1)))
                          (for [(i (in-range (- n 2) (sub1 0) -1))]
                            (set! stack
                                  (list-set stack (+ i 1) (list-ref stack i))))

                          (set! stack (list-set stack 0 t))
                          (set! j (sub1 j))
                          (loop)))]
                     [else
                      (let loop ()
                        (when (< j 0)
                          (define t (list-ref stack 0))
                          (for ([i (in-range (add1 n))])
                            (set! stack (list-set stack i (list-ref stack (+ i 1)))))

                          (set! stack (list-set stack (- n 1) t))))])]
                  [(34) ;; hflex
                   (define c1x (+ x (shift stack)))
                   (define c1y y)
                   (define c2x (+ c1x (shift stack)))
                   (define c2y (+ c1y (shift stack)))
                   (define c3x (+ c2x (shift stack)))
                   (define c3y c2y)
                   (define c4x (+ c3x (shift stack)))
                   (define c4y c3y)
                   (define c5x (+ c4x (shift stack)))
                   (define c5y c4y)
                   (define c6x (+ c5x (shift stack)))
                   (define c6y c5y)
                   (set! x c6x)
                   (set! y c6y)
                   (path-bezierCurveTo path c1x c1y c2x c2y c3x c3y)
                   (path-bezierCurveTo path c4x c4y c5x c5y c6x c6y)]

                  [(35) ;; flex
                   (define pts null)
                   (for ([i (in-range (add1 5))])
                     (set! x (+ x (shift stack)))
                     (set! y (+ y (shift stack)))
                     (push pts x y))

                   (apply path-bezierCurveTo path (take pts 6))
                   (apply path-bezierCurveTo path (drop pts 6))
                   (shift stack) ;; fd
                   ]

                  [(36) ;; hflex1
                   (define c1x (+ x (shift stack)))
                   (define c1y (+ y (shift stack)))
                   (define c2x (+ c1x (shift stack)))
                   (define c2y (+ c1y (shift stack)))
                   (define c3x (+ c2x (shift stack)))
                   (define c3y c2y)
                   (define c4x (+ c3x (shift stack)))
                   (define c4y c3y)
                   (define c5x (+ c4x (shift stack)))
                   (define c5y (+ c4y (shift stack)))
                   (define c6x (+ c5x (shift stack)))
                   (define c6y c5y)
                   (set! x c6x)
                   (set! y c6y)

                   (path-bezierCurveTo path c1x c1y c2x c2y c3x c3y)
                   (path-bezierCurveTo path c4x c4y c5x c5y c6x c6y)]

                  [(37) ;; flex1
                   (define startx x)
                   (define starty y)

                   (define pts null)
                   (for ([i (in-range 0 (add1 4))])
                     (set! x (+ x (shift stack)))
                     (set! y (+ y (shift stack)))
                     (push pts x y))

                   (cond
                     [(> (abs (- x startx)) (abs (- y starty))) ;; horzontal
                      (set! x (shift stack))
                      (set! y starty)]
                     [else
                      (set! x startx)
                      (set! y (shift stack))])

                   (push pts x y)
                   (apply path-bezierCurveTo path (take pts 6))
                   (apply path-bezierCurveTo path (drop pts 6))]
                  [else (error (format "unknown op: 12 ~a" op))])]
               [else (error (format "unknown op: ~a" op))])]
            [(< op 247) (push stack (- op 139))]
            [(< op 251)
             (define b1 (read-byte stream))
             (push stack (+ (* (- op 247) 256) b1 108))]
            [(< op 255)
             (define b1 (read-byte stream))
             (push stack (- (* (- op 251) 256) b1 108))]
            [else
             (push stack (/ (decode int32be stream) 65536))])
          (loop)))))

  (parse)

  (when open
    (path-closePath path))

  #R path
  (set-cff-glyph-path! this path)
  path)
