#lang racket/base
(require racket/match
         "struct.rkt")
(provide (all-defined-out))

(define-syntax-rule (define-guarded-parameter ID PRED STARTING-VALUE)
  (define ID
    (make-parameter STARTING-VALUE
                    (λ (val)
                      (unless (PRED val)
                        (raise-argument-error 'ID (format "~a" (object-name PRED)) val))
                      val))))

(define-guarded-parameter current-attrs (λ (xs) (and (list? xs) (andmap attr? xs))) null)
(define-guarded-parameter show-timing boolean? #false)
(define-guarded-parameter current-strict-attrs boolean? #false)
