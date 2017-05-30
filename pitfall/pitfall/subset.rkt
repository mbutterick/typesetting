#lang pitfall/racket
(provide Subset CFFSubset TTFSubset)

(define-subclass object% (Subset)
  (super-new))


(define-subclass Subset (CFFSubset font)
  (super-new)
  (error 'cff-subset-unimplemented))


(define-subclass Subset (TTFSubset font)
  (super-new)
  (error 'ttf-subset-unimplemented))