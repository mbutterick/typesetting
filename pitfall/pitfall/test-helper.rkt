#lang racket/base
(require rackunit pitfall/helper)
(provide (all-defined-out))

(define-syntax-rule (check-exn-equal? expr val)
    (check-equal? (with-handlers ([exn:pitfall:test? (λ (e) (exn:pitfall:test-data e))])
                    expr) val))