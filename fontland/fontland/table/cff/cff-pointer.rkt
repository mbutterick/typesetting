#lang debug racket/base
(require racket/class racket/list xenomorph/pointer xenomorph/base "cff-struct.rkt")
(provide CFFPointer)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFPointer.js
|#


(define (CFFPointer type
                    #:relative-to [relative-to 'global]
                    #:lazy [lazy #false])
  (x:pointer #:type type
             #:base-class CFFPointer%
             #:relative-to relative-to
             #:lazy lazy))

(define CFFPointer%
  (class x:pointer%
    (super-new)

    (inherit/super [%encode encode])
    (inherit-field type offset-type)
    
    (define/override (decode stream parent operands)
      (set! offset-type (make-object
                            (class x:base%
                              (super-new)
                              (define/augment (decode . args) (first operands)))))
      (super decode stream parent))

    (override [@encode encode])
    (define (@encode value stream ctx)
      (cond
        [(not stream)
         ;; compute the size (so ctx.pointerSize is correct)
         (set! offset-type (make-object
                               (class x:base%
                                 (super-new)
                                 (define/augment (size . args) 0))))
         (send this size value ctx)
         (list (Ptr 0))]
        [else
         #RRR 'rees
         (define ptr #false)
         (set! offset-type (make-object
                               (class x:base%
                                 (super-new)
                                 (define/augment (encode val stream . _) (set! ptr val)))))
         (pointer-encode this value stream ctx)
         (list (Ptr ptr))]))))

