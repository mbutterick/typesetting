#lang debug racket/base
(require racket/class racket/list xenomorph "cff-struct.rkt")
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
         (define ptr #false)
         (set! offset-type (make-object
                               (class x:base%
                                 (super-new)
                                 (define/augment (encode val stream) (set! ptr val)))))
         (error 'branch-not-impl)
         #;(super encode value stream ctx)
         (list (Ptr ptr))]))))

