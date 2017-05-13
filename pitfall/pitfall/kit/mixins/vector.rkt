#lang racket/base
(require racket/class)
(provide vector-mixin)

(define (vector-mixin %)
  (class %
    (super-new)
    (field [(@_ctm _ctm) '(1 0 0 1 0 0)]
           [(@_ctmStack _ctmStack) null])
    
    (define/public (initVector) (void))
    
    (define/public (transform m11 m12 m21 m22 dx dy)
      (send this addContent "from vector/transform")))) ; todo