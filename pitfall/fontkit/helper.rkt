#lang racket/base
(require (for-syntax racket/base) racket/runtime-path br/define)
(provide (all-defined-out))

(define index? (λ (x) (and (number? x) (integer? x) (not (negative? x)))))

(define-runtime-path charter-path "../pitfall/test/assets/charter.ttf")
(define-runtime-path charter-italic-path "../pitfall/test/assets/charter-italic.ttf")
(define-runtime-path charter-directory-path "charter-directory.rktd")
(define-runtime-path charter-italic-directory-path "charter-italic-directory.rktd")

(define-macro (test-module . EXPRS)
  #`(module+ test
      (require #,(datum->syntax caller-stx 'rackunit) #,(datum->syntax caller-stx 'racket/serialize)) 
      . EXPRS))