#lang racket/base
(require (for-syntax racket/base) racket/runtime-path br/define)
(provide (all-defined-out))

(define index? (λ (x) (and (number? x) (integer? x) (not (negative? x)))))

(define-runtime-path charter-path "../pitfall/test/assets/Charter.ttf")

(define-macro (test-module . EXPRS)
  #`(module+ test
      (require #,(datum->syntax caller-stx 'rackunit))
      . EXPRS))