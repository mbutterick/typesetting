#lang racket/base
(require racket/contract)
(provide (all-defined-out))

(define-syntax-rule (define-pass (PASS-NAME ARG OTHER-ARG ...)
                      #:precondition PRECOND-PROC
                      #:postcondition POSTCOND-PROC
                      EXPRS ...)
  (define (PASS-NAME ARG OTHER-ARG ...)
    (unless (PRECOND-PROC ARG)
      (error 'PASS-NAME (format "precondition failed: ~a" 'PRECOND-PROC)))
    (define res (let () EXPRS ...))
    (unless (POSTCOND-PROC res)
      (error 'PASS-NAME (format "postcondition failed: ~a" 'POSTCOND-PROC)))
    res))