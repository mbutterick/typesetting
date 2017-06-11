#lang racket/base
(require (for-syntax racket/base br/syntax) racket/class br/define)
(provide (all-defined-out))

(define RBase
  (class object%
    (super-new)    
    (abstract decode)
    (abstract encode)
    (abstract size)
    (define/public (process . args) (void))
    (define/public (preEncode . args) (void))))


(define-macro (test-module . EXPRS)
  #`(module+ test
      (require #,(datum->syntax caller-stx 'rackunit))
      . EXPRS))


(define (port-position port)
  (define-values (l c p) (port-next-location port))
  p)


