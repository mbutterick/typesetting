#lang debug racket
(require racket/class racket/match xenomorph sugar/unstable/dict)
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

    (augride [@decode decode])
    (define (@decode stream parent)
      (match (decode (if (>= (getCFFVersion parent) 2) uint32be uint16be) stream)
        [0 null]
        [count  (define offSize (decode uint8 stream))
               (define offsetType (match offSize
                                    [1 uint8]
                                    [2 uint16be]
                                    [3 uint24be]
                                    [4 uint32be]
                                    [_ (error (format "bad-offset-size-in-CFFIndex ~a" offSize))]))
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
      (define size 2)
      (cond
        [(zero? (length arr)) size]
        [else
         (define type (or @type (x:buffer)))

         ;; find maximum offset to determinine offset type
         (define offset 1)
         (for ([(item i) (in-indexed arr)])
           (set! offset (+ offset (send type size item parent))))

         (define offsetType
           (cond
             [(<= offset #xff) uint8]
             [(<= offset #xffff) uint16be]
             [(<= offset #xffffff) uint24be]
             [(<= offset #xffffffff) uint32be]
             [else (error 'CFFIndex-size (format "bad offset: ~a" offset))]))

         (set! size (+ size 1 (* (send offsetType size) (add1 (length arr)))))
         (set! size (+ size (sub1 offset)))

         size]))

    (augride [@encode encode])
    (define (@encode arr stream parent)
      (send uint16be encode (length arr) stream)
      (cond
        [(zero? (length arr))]
        [else
         (define type (or @type (x:buffer)))

         ;; find maximum offset to detminine offset type
         (define sizes null)
         (define offset 1)
         (for ([item (in-list arr)])
           (define s (send type size item parent))
           (set! sizes (append sizes (list s)))
           (set! offset (+ offset s)))

         (define offsetType
           (cond
             [(<= offset #xff)
              uint8]
             [(<= offset #xffff)
              uint16be]
             [(<= offset #xffffff)
              uint24be]
             [(<= offset #xffffffff)
              uint32be]
             [else
              (error 'cff-index-encode-bad-offset!)]))

         ;; write offset size
         (send uint8 encode (size offsetType) stream)

         ;; write elements
         (set! offset 1)
         (send offsetType encode offset stream)

         (for ([size (in-list sizes)])
           (set! offset (+ offset size))
           (send offsetType encode offset stream))

         (for ([item (in-list arr)])
           (send type encode item stream parent))]))))

(define (CFFIndex [type #f])
  (new CFFIndex% [type type]))