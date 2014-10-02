#lang racket/base
(require racket/class "helper.rkt")
(provide (all-defined-out))

(define Variable
  (class* object% (printable<%>) 
    (super-new)
    (define (repr) (format "<Variable ~a>" _name))
    (define/public (custom-print out quoting-depth) (print (repr) out))
    (define/public (custom-display out) (displayln (repr) out))
    (define/public (custom-write out) (write (repr) out))
    
    (init-field name)
    (field [_name name])))
(define Variable? (is-a?/c Variable))

(define Unassigned (new Variable [name "Unassigned"]))