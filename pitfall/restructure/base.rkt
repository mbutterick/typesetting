#lang racket/base
(require racket/class sugar/debug)
(provide (all-defined-out))

(define RestructureBase
  (class object%
    (super-new)    
    (field [starting-offset #f]
           [parent #f])
    (define/pubment (decode stream . args)
      (set! starting-offset (and (object? stream) (send stream pos)))
      (set! parent (and (pair? args) (is-a? (car args) RestructureBase) (car args)))
      (inner (void) decode stream . args))
    (abstract encode)
    (abstract size)
    (define/public (process . args) (void))
    (define/public (preEncode . args) (void))))

(define (RestructureBase? x) (is-a? x RestructureBase))