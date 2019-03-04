#lang racket/base
(require racket/class xenomorph sugar/unstable/dict)
(provide CFFIndex)

(define CFFIndex%
  (class xenobase%
    (super-new)
    (init-field [type type])

    (define (getCFFVersion ctx)
      (let loop ([ctx ctx])
        (if (and ctx (not (hash-ref ctx 'hdrSize)))
            (loop (hash-ref ctx 'parent))
            (if ctx (hash-ref ctx 'version) -1))))
    
    (define/augride (:decode stream parent)
      (define version (getCFFVersion parent))
      (define count (decode (if (>= version 2)
                                uint32be
                                uint16be) stream))
      count
      )))

(define (CFFIndex type)
  (new CFFIndex% [type type]))