#lang racket/base
(require (for-syntax racket/base) racket/runtime-path br/define)
(provide (all-defined-out))

(define index? (λ (x) (and (number? x) (integer? x) (not (negative? x)))))

(define-runtime-path charter-path "../ptest/assets/charter.ttf")
(define-runtime-path charter-italic-path "../ptest/assets/charter-italic.ttf")
(define-runtime-path fira-path "../ptest/assets/fira.ttf")
(define-runtime-path fira-otf-path "../ptest/assets/fira.otf")
(define-runtime-path charter-directory-path "charter-directory.rktd")
(define-runtime-path charter-italic-directory-path "charter-italic-directory.rktd")

(define-macro (test-module . EXPRS)
  #`(module+ test
      (require #,(datum->syntax caller-stx 'rackunit) #,(datum->syntax caller-stx 'racket/serialize)) 
      . EXPRS))


(define (is-mark? codepoint)
  ;; mark classes = Mn Me Mc
  (regexp-match #px"\\p{Mn}|\\p{Me}|\\p{Mc}" (string (integer->char codepoint))))

(module+ test
  (require rackunit)
  (check-true (and (is-mark? #x300) #t))
  (check-false (and (is-mark? #x2ee) #t)))