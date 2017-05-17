#lang pitfall/racket
(require "image.rkt")
(provide image-mixin)

(define (image-mixin [% mixin-tester%])
  (class %
    (super-new)
    #;(field [_opacityRegistry #f]
           [_opacityCount #f]
           [_gradCount #f]
           [_fillColor #f])

    (as-methods
    )))

    
#;(define/contract (initColor this)
  (->m void?)
  (set-field! _opacityRegistry this (mhash))
  (set-field! _opacityCount this 0)
  (set-field! _gradCount this 0))
