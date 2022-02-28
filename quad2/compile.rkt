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
                      #:pre PRECOND-PROC
                      #:post POSTCOND-PROC
                      EXPRS ...)
  (define PASS-NAME
    (make-compiler
     (list (procedure-rename
            (λ (ARG OTHER-ARG ...)
              (unless (PRECOND-PROC ARG)
                (raise-argument-error 'PASS-NAME (format "~a" 'PRECOND-PROC) ARG))
              (define res (let () EXPRS ...))
              (unless (POSTCOND-PROC res)
                (raise-argument-error 'PASS-NAME (format "~a" 'POSTCOND-PROC) res))
              res) 'PASS-NAME)))))