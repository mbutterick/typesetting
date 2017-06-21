#lang racket/base
(require (for-syntax racket/base br/syntax) racket/class br/define "base.rkt")
(provide (all-defined-out) (all-from-out "base.rkt"))


(define-macro (test-module . EXPRS)
  #`(module+ test
      (require #,(datum->syntax caller-stx 'rackunit) #,(datum->syntax caller-stx 'racket/serialize))
      . EXPRS))

(define index? (λ (x) (and (number? x) (integer? x) (not (negative? x)))))