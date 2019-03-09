#lang debug racket
(require racket/class racket/match xenomorph sugar/unstable/dict)
(provide CFFIndex)

(define CFFIndex%
  (class x:base%
    (super-new)
    (init-field [(@type type) #f])

    (define (getCFFVersion ctx)
      (let loop ([ctx ctx])
        (cond
          [(and ctx (hash? ctx) (not (hash-ref ctx 'hdrSize #f)))
           (loop (hash-ref ctx 'x:parent))]
          [(and ctx (hash-ref ctx 'x:version #f))]
          [else -1])))

    (augride [@decode decode])
    (define (@decode stream parent)
      (match (decode (if (>= (getCFFVersion parent) 2) uint32be uint16be) stream)
        [0 null]
        [count (define offSize (decode uint8 stream))
               (define offsetType (match offSize
                                    [1 uint8]
                                    [2 uint16be]
                                    [3 uint24be]
                                    [4 uint32be]
                                    [_ (error 'bad-offset-size-in-CFFIndex)]))
               (define startPos (+ (pos stream) (* (add1 count) offSize) -1))
               (for/fold ([vals null]
                          [start (send offsetType decode stream)]
                          #:result (begin0 (reverse vals) (pos stream (+ startPos start))))
                         ([i (in-range count)])
                 (define end (send offsetType decode stream))
                 (define val
                   (cond
                     [@type
                      (define apos (pos stream))
                      (pos stream (+ startPos start))
                      (hash-set! parent 'length (- end start))
                      (begin0
                        (send @type decode stream parent)
                        (pos stream apos))]
                     [else
                      (hasheq 'offset (+ startPos start)
                              'length (- end start))]))
                 (values (cons val vals) end))]))

    (augride [@size size])
    (define (@size arr parent)
      #RRR 'in-cff-index-size
      #R (length arr)
      (when (= 819 (length arr))
        #R (take arr 3))
      (define size 2)
      (cond
        [(zero? (length arr)) size]
        [else
         (define type (or @type (x:buffer)))

         ;; find maximum offset to determinine offset type
         (define offset 1)
         (for ([(item i) (in-indexed arr)])
              (set! offset (+ offset #R (send #R type size item parent))))

         #RR offset

         (define offsetType
           (cond
             [(<= offset #xff) uint8]
             [(<= offset #xffff) uint16be]
             [(<= offset #xffffff) uint24be]
             [(<= offset #xffffffff) uint32be]
             [else (error 'CFFIndex-size (format "bad offset: ~a" offset))]))

         (set! size (+ size 1 (* (send offsetType size) (add1 (length arr)))))
         (set! size (+ size (sub1 offset)))

         #RR size]))

    (define/augride (encode arr stream parent)
      (error 'cff-index-encode-not-implemented))))

(define (CFFIndex [type #f])
  (new CFFIndex% [type type]))