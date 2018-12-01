#lang racket
(require racket/runtime-path)
(provide (all-defined-out))

(define-syntax-rule (r+p ID ...)
  (begin (require ID ...) (provide (all-from-out ID ...))))

(define index? (λ (x) (and (number? x) (integer? x) (not (negative? x)))))

(define-runtime-path charter-path "assets/charter.ttf")
(define-runtime-path charter-italic-path "assets/charter-italic.ttf")
(define-runtime-path fira-path "assets/fira.ttf")
(define-runtime-path fira-otf-path "assets/fira.otf")
(define-runtime-path charter-directory-path "assets/charter-directory.rktd")
(define-runtime-path charter-italic-directory-path "assets/charter-italic-directory.rktd")

(define-syntax (test-module stx)
  (syntax-case stx ()
    [(_ . EXPRS)
     #`(module+ test
         (require #,(datum->syntax stx 'rackunit) #,(datum->syntax stx 'racket/serialize)) 
         . EXPRS)]))


(define (is-mark? codepoint)
  ;; mark classes = Mn Me Mc
  (regexp-match #px"\\p{Mn}|\\p{Me}|\\p{Mc}" (string (integer->char codepoint))))

(module+ test
  (require rackunit)
  (check-true (and (is-mark? #x300) #t))
  (check-false (and (is-mark? #x2ee) #t)))