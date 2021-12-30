#lang debug racket
(require racket/class racket/match xenomorph sugar/unstable/dict fontland/struct)
(provide CFFIndex)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFIndex.js
|#

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

    (define/augride (x:decode stream parent)
      (match (decode (if (>= (getCFFVersion parent) 2) uint32be uint16be) stream)
        [0 (vector)]
        [count  (define offSize (decode uint8 stream))
                (define offsetType (match offSize
                                     [1 uint8]
                                     [2 uint16be]
                                     [3 uint24be]
                                     [4 uint32be]
                                     [_ (error (format "bad-offset-size-in-CFFIndex ~a" offSize))]))
                (define startPos (+ (pos stream) (* (add1 count) offSize) -1))
                (for/fold ([vals null]
                           [start (send offsetType x:decode stream)]
                           #:result (begin0 (list->vector (reverse vals)) (pos stream (+ startPos start))))
                          ([i (in-range count)])
                  (define end (send offsetType x:decode stream))
                  (define val
                    (cond
                      [@type
                       (define apos (pos stream))
                       (pos stream (+ startPos start))
                       (hash-set! parent 'length (- end start))
                       (begin0
                         (send @type x:decode stream parent)
                         (pos stream apos))]
                      [else
                       (index-item (+ startPos start) (- end start))]))
                  (values (cons val vals) end))]))

    (define/augride (x:size arr-arg parent)
      (define arr (match arr-arg
                [(? list? xs) (list->vector xs)]
                [vec vec]))
      (+ 2
         (cond
           [(zero? (vector-length arr)) 0]
           [else (define type (or @type (x:buffer)))

                 ;; find maximum offset to determinine offset type
                 (define offset
                   (add1 (for/sum ([item (in-vector arr)])
                                  (send type x:size item parent))))

                 (define offset-type
                   (cond
                     [(<= offset #xff) uint8]
                     [(<= offset #xffff) uint16be]
                     [(<= offset #xffffff) uint24be]
                     [(<= offset #xffffffff) uint32be]
                     [else (error 'CFFIndex-size (format "bad offset: ~a" offset))]))

                 (+ (* (send offset-type x:size) (add1 (vector-length arr))) offset)])))

    (define/augride (x:encode arr-arg stream parent)
      (define arr (match arr-arg
                [(? list? xs) (list->vector xs)]
                [vec vec]))
      (send uint16be x:encode (vector-length arr) stream)
      (cond
        [(zero? (vector-length arr))]
        [else
         (define type (or @type (x:buffer)))

         ;; find maximum offset to detminine offset type
         (define-values (sizes offset)
           (for/fold ([sizes null]
                      [offset 1]
                      #:result (values (reverse sizes) offset))
                     ([item (in-vector arr)])
             (define s (send type x:size item parent))
             (values (cons s sizes) (+ offset s))))

         (define offsetType
           (cond
             [(<= offset #xff) uint8]
             [(<= offset #xffff) uint16be]
             [(<= offset #xffffff) uint24be]
             [(<= offset #xffffffff) uint32be]
             [else (error 'cff-index-encode-bad-offset!)]))

         ;; write offset size
         (send uint8 x:encode (send offsetType x:size) stream)

         ;; write elements
         (for/fold ([offset 1])
                   ([size (in-list (cons 0 sizes))])
           (define next-offset (+ offset size))
           (send offsetType x:encode next-offset stream)
           next-offset)

         (for ([item (in-vector arr)])
              (send type x:encode item stream parent))]))))

(define (CFFIndex [type #f])
  (new CFFIndex% [type type]))