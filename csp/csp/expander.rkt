#lang br/quicklang
(require csp racket/stxparam racket/splicing)
(provide (all-defined-out)
          (except-out (all-from-out br/quicklang) #%module-begin)
         (rename-out [mb #%module-begin]))

(define-syntax-parameter PROB (λ (stx) (error 'not-parameterized)))
(define-syntax-parameter SOLVE (make-rename-transformer #'solve))

(define-macro (mb EXPR0 ... #:output ID EXPR ...)
  (with-syntax ([prob #'ID])
    #'(#%module-begin
       (require csp)
       (provide prob SOLVE)
       (define prob (make-csp))
       (println prob)
       (splicing-syntax-parameterize ([PROB (make-rename-transformer #'ID)])
         EXPR0 ... 
         EXPR ...))))

(define-macro (define-variable VID DOMAIN)
    #'(begin
        (define VID DOMAIN)
        (add-var! PROB 'VID DOMAIN)))

(define-macro (define-constraint CID FUNC VARSYMS)
  #'(begin
      (define CID (constraint FUNC VARSYMS))
      (add-constraint! PROB FUNC VARSYMS)))