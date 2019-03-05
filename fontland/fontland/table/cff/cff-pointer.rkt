#lang racket/base
(require racket/class racket/list xenomorph)
(provide CFFPointer)

#|
approximates
https://github.com/mbutterick/fontkit/blob/master/src/cff/CFFPointer.js
|#

(define CFFPointer
  (class x:pointer%
    (super-new)

    (inherit-field offset-type)
    (define ptr #false)
    
    (define/override (:decode stream parent operands)
      (set! offset-type (class xenobase%
                          (super-new)
                          (define/augment (:decode . args) (first operands))
                          ;; compute the size (so ctx.pointerSize is correct)
                          (define/augment (:size . args) 0)
                          (define/augment (:encode stream val) (set! ptr val))))

      (super :decode stream parent operands))

    (define/override (:encode stream value ctx)
      (cond
        [(not stream)
         (send this :size value ctx)
         (Ptr 0)]
        [else
         (super :encode stream value ctx)
         (Ptr ptr)]))))

(struct Ptr (val [forceLarge #:auto]) #:transparent #:mutable #:auto-value #true
  ;; use prop:procedure instead of JS `valueOf`
  #:property prop:procedure (λ (ptr) (Ptr-val ptr)))