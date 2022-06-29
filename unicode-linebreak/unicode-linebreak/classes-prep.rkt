#lang debug racket/base
(require  (for-syntax racket/base) racket/match racket/string)

(module+ reader
  (require syntax/strip-context)
  (provide (rename-out [rs read-syntax]))
  (define (rs name ip)
    (define lines
      (for*/list ([line (in-lines ip)]
                  [str (in-value (string-trim (string-trim line #px"#.*" #:left? #false)))]
                  #:when (non-empty-string? str))
        (match-define (list range tag) (string-split str ";"))
        (list (map (λ (str) (string->number str 16)) (string-split range ".."))
              (string->symbol tag))))
    (strip-context
     (with-syntax ([LINES lines])
       #'(module _ "classes-prep.rkt"
           . LINES)))))

(define-syntax (make-cond stx)
  (syntax-case stx ()
    [(_ ID VAL) #'(eq? ID VAL)] ;; I believe `eq?` is OK because a codepoint is a fixnum
    [(_ ID LVAL RVAL) #'(<= LVAL ID RVAL)]))

(provide (rename-out [mb #%module-begin])
         (except-out (all-from-out racket/base) #%module-begin))
(define-syntax (mb stx)
  (syntax-case stx ()
    [(_ (VALS RES) ...)
     (with-syntax ([F (datum->syntax stx 'f)])
       #'(#%module-begin
          (provide F)
          (define (F x)
            (cond [(make-cond x . VALS) 'RES] ...))))]))