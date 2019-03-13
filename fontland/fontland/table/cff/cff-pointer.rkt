#lang debug racket/base
(require racket/class racket/list xenomorph/pointer xenomorph/base "cff-struct.rkt")
(provide CFFPointer CFFPointer%)

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

    (inherit-field type offset-type)
    
    (define/override (x:decode stream parent operands)
      (set! offset-type (make-object
                            (class x:base%
                              (super-new)
                              (define/augment (x:decode . args) (first operands)))))
      (super x:decode stream parent))

    (define/override (x:encode value stream ctx)
      (cond
        [(not stream)
         ;; compute the size (so ctx.pointerSize is correct)
         (set! offset-type (make-object
                               (class x:base%
                                 (super-new)
                                 (define/augment (x:size . args) 0))))
         (send this x:size value ctx)
         (list (Ptr 0))]
        [else
         (define ptr #false)
         (set! offset-type (make-object
                               (class x:base%
                                 (super-new)
                                 (define/augment (x:encode val stream . _) (set! ptr val)))))
         (super x:encode value stream ctx)
         (list (Ptr ptr))]))))

