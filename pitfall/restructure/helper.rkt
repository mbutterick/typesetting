#lang racket/base
(require (for-syntax racket/base br/syntax) racket/class br/define)
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


(define-macro (test-module . EXPRS)
  #`(module+ test
      (require #,(datum->syntax caller-stx 'rackunit))
      . EXPRS))

(define index? (λ (x) (and (number? x) (integer? x) (not (negative? x)))))