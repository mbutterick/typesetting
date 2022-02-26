#lang debug racket/base
(require racket/match
         "quad.rkt")
(provide (all-defined-out))

(struct compiler (passes)
  #:constructor-name make-compiler
  #:guard (λ (procs name)
            (unless ((list-of procedure?) procs)
              (raise-argument-error 'bad-input-to-compiler-constructor "list of procedures" procs))
            procs)
  #:property prop:procedure
  (λ (self input) ((apply compose1 (reverse (compiler-passes self))) input)))

(define (compiler-append c passes)
  (make-compiler (append (compiler-passes c) passes)))

(define-syntax-rule (define-pass (PASS-NAME ARG OTHER-ARG ...)
                      #:precondition PRECOND-PROC
                      #:postcondition POSTCOND-PROC
                      EXPRS ...)
  (define PASS-NAME
    (make-compiler (list (λ (ARG OTHER-ARG ...)
                           (unless (PRECOND-PROC ARG)
                             (error 'PASS-NAME (format "precondition failed: ~a for value ~v" 'PRECOND-PROC ARG)))
                           (define res (let () EXPRS ...))
                           (unless (POSTCOND-PROC res)
                             (error 'PASS-NAME (format "postcondition failed: ~a for value ~v" 'POSTCOND-PROC res)))
                           res)))))