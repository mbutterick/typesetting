#lang debug racket/base
(require racket/class racket/match xenomorph sugar/unstable/dict)
(provide CFFIndex)

(define CFFIndex%
  (class x:base%
    (super-new)
    (init-field [(@type type) #f])

    (define (getCFFVersion ctx)
      (let loop ([ctx ctx])
        (if (and ctx
                 (hash? ctx)
                 (hash-has-key? ctx 'hdrSize)
                 (not (hash-ref ctx 'hdrSize)))
            (loop (hash-ref ctx 'parent))
            (if ctx (hash-ref ctx 'x:version) -1))))

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
                          [start (decode offsetType stream)]
                          #:result (begin0 (reverse vals) (pos stream (+ startPos start))))
                         ([i (in-range count)])
                 (define end (decode offsetType stream))
                 (define val
                   (cond
                     [@type
                      (define apos (pos stream))
                      (pos stream (+ startPos start))
                      (hash-set! parent 'length (- end start))
                      (begin0
                        (decode @type stream #:parent parent)
                        (pos stream apos))]
                     [else
                      (hasheq 'offset (+ startPos start)
                              'length (- end start))]))
                 (values (cons val vals) end))]))

    (define/augride (size arr parent)
      (error 'cff-index-size-not-implemented))

    (define/augride (encode stream arr parent)
      (error 'cff-index-encode-not-implemented))))

(define (CFFIndex [type #f])
  (new CFFIndex% [type type]))