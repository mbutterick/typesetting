#lang racket/base
(require racket/class sugar/debug)
(provide (all-defined-out))

(define RestructureBase
  (class object%
    (super-new)    
    (field [_startOffset #f]
           [_currentOffset #f]
           [_length #f]
           [parent #f])
    (define/public (decode stream . args)
      (set! _startOffset (and (object? stream) (send stream pos)))
      (set! parent (and (pair? args) (is-a? (car args) RestructureBase) (car args)))
      #;(inner (void) decode stream . args))
    (define/public (encode . xs) (void))
    (define/public (size . xs) (void))
    (define/public (process . args) (void))
    (define/public (preEncode . args) (void))))

(define (RestructureBase? x) (is-a? x RestructureBase))