#lang debug racket/base
(require racket/match
         racket/string
         (for-syntax racket/base)
         "param.rkt"
         "quad.rkt")
(provide (all-defined-out))

(define (list-of proc)
  (λ (x)
    (and (list? x)
         (for/and ([xi (in-list x)])
           (or (proc xi)
               (let ([procname (object-name proc)])
                 (raise-argument-error
                  (string->symbol (format "list-of ~a" procname))
                  (symbol->string procname) xi)))))))

(struct pipeline (passes)
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

(define (make-pipeline . passes)
  (pipeline passes))

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
          (procedure-rename
           #,(syntax/loc stx
               (λ (ARG)
                 (define ((make-failure-handler failure-msg) exn)
                   (raise (make-exn:fail:contract
                           (string-replace (exn-message exn) "contract violation" (string-append "contract violation in " failure-msg)) (exn-continuation-marks exn))))
                 
                 (when (current-use-preconditions?)
                   (define failure-msg (format "~a pass (as precondition)" 'PASS-NAME))
                   (with-handlers ([exn:fail:contract? (make-failure-handler failure-msg)])
                     (unless (PRECOND-PROC ARG)
                       (raise-argument-error 'PASS-NAME (symbol->string 'PRECOND-PROC) ARG))))
                 ;; a pass can be functional or mutational.
                 ;; if it returns void, assume mutational
                 ;; and return the input item.
                 (define res (match (let () EXPRS ...)
                               [(? void?) ARG]
                               [val val]))
                 (begin0
                   res
                   (when (current-use-postconditions?)
                     (define failure-msg (format "~a pass (as postcondition)" 'PASS-NAME))
                     (with-handlers ([exn:fail:contract? (make-failure-handler failure-msg)])
                       (unless (POSTCOND-PROC res)
                         (raise-argument-error 'PASS-NAME (symbol->string 'POSTCOND-PROC) ARG)))))))
           'PASS-NAME)))]))

(define-pass (print-pass qs)
  #:pre values
  #:post values
  (for-each println qs))