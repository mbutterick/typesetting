#lang racket/base
(require racket/contract racket/class)
(provide (all-defined-out))

(define (option/c x) (or/c #f x))

(module+ main

  (define-syntax-rule (define/public/contract (ID . ARGS) CONTRACT . BODY)
    (define/public (ID . ARGS)
      (define/contract (ID . ARGS)
        CONTRACT . BODY)
      (ID . ARGS)))

  (define c% (class object%
               (super-new)
             
               (define/public/contract (add x y)
                 (integer? integer? . -> . integer?)
                 (+ x y))))


  (define c (make-object c%))

  (send c add 12 21))