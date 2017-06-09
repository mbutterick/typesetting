#lang racket/base
(require (for-syntax racket/base br/syntax) racket/class br/define)
(provide (all-defined-out))

(define RBase
  (class object%
    (super-new)    
    (abstract decode)
    (abstract encode)
    (define/public (process . args)
      (void))
    #;(abstract size)))


(define-macro (test-module . EXPRS)
  #`(module+ test
      (require #,(datum->syntax caller-stx 'rackunit))
      . EXPRS))


(define (port-position port)
  (define-values (l c p) (port-next-location port))
  p)


