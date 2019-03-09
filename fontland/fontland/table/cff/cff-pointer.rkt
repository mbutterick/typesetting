#lang debug racket/base
(require racket/class racket/list xenomorph)
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
    (inherit/super encode)
    (define (@encode value stream ctx)
      #R (get-field offset-type this)
      #R (get-field type this)
      #R stream
      (cond
        [#R (not stream)
         ;; compute the size (so ctx.pointerSize is correct)
         (set! offset-type (make-object
                               (class x:base%
                                 (super-new)
                                 (define/augment (size . args) 0))))
         (send this size value ctx)
         (list (Ptr 0))]
        [else
         #R value
         #R stream
         (define ptr #false)
         (set! offset-type (make-object
                               (class x:base%
                                 (super-new)
                                 (define/augment (encode val stream) (set! ptr val)))))
         (super encode value stream ctx)
         (list (Ptr ptr))]))))



(struct Ptr (val [forceLarge #:auto]) #:transparent #:mutable #:auto-value #true
  ;; use prop:procedure instead of JS `valueOf`
  #:property prop:procedure (λ (ptr) (Ptr-val ptr)))