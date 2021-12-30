#lang racket
(require racket/runtime-path)
(provide (all-defined-out))

(define-syntax-rule (r+p ID ...)
  (begin (require ID ...) (provide (all-from-out ID ...))))

(define index? (λ (x) (and (number? x) (integer? x) (not (negative? x)))))

(define-runtime-path charter-path "assets/charter.ttf")
(define-runtime-path charter-woff-path "assets/charter.woff")
(define-runtime-path charter-italic-path "assets/charter-italic.ttf")
(define-runtime-path fira-path "assets/fira.ttf")
(define-runtime-path fira-otf-path "assets/fira.otf")
(define-runtime-path charter-directory-path "assets/charter-directory.rktd")
(define-runtime-path charter-italic-directory-path "assets/charter-italic-directory.rktd")
(define-runtime-path fira-otf-directory-path "assets/fira-otf-directory.rktd")