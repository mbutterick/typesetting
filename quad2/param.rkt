#lang racket/base
(require racket/match
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
  [current-attrs (λ (xs) (and (list? xs) (andmap attr? xs))) null]
  [current-show-timing? boolean? #false]
  [current-strict-attrs? boolean? #false]
  [current-use-preconditions? boolean? #true]
  [current-use-postconditions? boolean? #true])
