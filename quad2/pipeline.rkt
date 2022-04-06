#lang debug racket/base
(require racket/match
         (for-syntax racket/base)
         "param.rkt"
         "quad.rkt")
(provide (all-defined-out))

(struct pipeline (passes)
  #:constructor-name make-pipeline
  #:guard (λ (procs name)
            (unless ((list-of procedure?) procs)
              (raise-argument-error 'bad-input-to-compiler-constructor "list of procedures" procs))
            procs)
  #:property prop:procedure
  (λ args
    (match-define (list* pipeline pass-arg _) args)
    (let ([show-timing? (current-show-timing?)])
      (for/fold ([pass-arg pass-arg])
                ([pass (in-list (pipeline-passes pipeline))])
        (define thunk (λ () (pass pass-arg)))
        (if show-timing?
            (time (displayln pass) (thunk))
            (thunk))))))

(define (compiler-append c passes)
  (make-pipeline (append (pipeline-passes c) passes)))

(define-syntax (define-pass stx)
  (syntax-case stx ()
    [(_ (PASS-NAME ARG OTHER-ARG ...)
        #:pre PRECOND-PROC
        #:post POSTCOND-PROC
        EXPRS ...)
     #`(define PASS-NAME
         (make-pipeline
          (list
           (procedure-rename
            #,(syntax/loc stx
                (λ (ARG)
                  (when (current-use-preconditions?)
                    (unless (PRECOND-PROC ARG)
                      (raise-argument-error 'PASS-NAME (format "~a (as precondition)" 'PRECOND-PROC) ARG)))
                  ;; a pass can be functional or mutational.
                  ;; if it returns void, assume mutational
                  ;; and return the input item.
                  (define res (match (let () EXPRS ...)
                                [(? void?) ARG]
                                [val val]))
                  (begin0
                    res
                    (when (current-use-postconditions?)
                      (unless (POSTCOND-PROC res)
                        (raise-argument-error 'PASS-NAME (format "~a (as postcondition)" 'POSTCOND-PROC) res))))))
            'PASS-NAME))))]))