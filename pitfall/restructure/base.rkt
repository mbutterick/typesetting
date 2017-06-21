#lang racket/base
(require racket/class)
(provide (all-defined-out))

(define RestructureBase
  (class object%
    (super-new)    
    (abstract decode)
    (abstract encode)
    (abstract size)
    (define/public (process . args) (void))
    (define/public (preEncode . args) (void))))

(define (RestructureBase? x) (is-a? x RestructureBase))