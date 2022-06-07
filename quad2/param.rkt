#lang racket/base
(require racket/match
         "constants.rkt"
         "attr.rkt"
         "struct.rkt")
(provide (all-defined-out))

(define-syntax-rule (define-guarded-parameters [ID PRED STARTING-VALUE] ...)
  (begin
    (define ID
      (make-parameter STARTING-VALUE
                      (λ (val)
                        (unless (PRED val)
                          (raise-argument-error 'ID (format "~a" (object-name PRED)) val))
                        val))) ...))

(define-guarded-parameters
  [current-attr-keys (λ (xs) (and (list? xs) (andmap attr-key? xs))) all-attr-keys]
  [current-show-timing? boolean? #false]
  [current-strict-attrs? boolean? #false]
  [current-use-preconditions? boolean? #true]
  [current-use-postconditions? boolean? #true])


(define (number-or-false? x) (or (eq? #false x) (number? x)))

(define-guarded-parameters
  [debug-page-width number-or-false? #false]
  [debug-page-height number-or-false? #false])
