#lang racket/base
(require racket/class "helper.rkt" "util.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Reserved.coffee
|#

(define xreserved%
  (class xenobase%
    (super-new)
    (init-field [(@type type)] [(@count count)])

    (unless (xenomorphic-type? @type)
      (raise-argument-error '+xoptional"xenomorphic type" @type))

    (define/augment (xxdecode port parent)
      (pos port (+ (pos port) (xxsize #f parent)))
      (void))

    (define/augment (xxencode val port [parent #f])
      (make-bytes (xxsize val parent) 0))
    
    (define/augment (xxsize [val #f] [parent #f])
      (* (send @type xxsize) (resolve-length @count #f #:parent parent)))))

(define (+xreserved [type-arg #f] [count-arg #f]
                    #:type [type-kwarg #f]
                    #:count [count-kwarg #f]
                    #:pre-encode [pre-proc #f]
                    #:post-decode [post-proc #f])
  (define type (or type-arg type-kwarg))
  (define count (or count-arg count-kwarg 1))
  (new (generate-subclass xreserved% pre-proc post-proc) [type type] [count count]))