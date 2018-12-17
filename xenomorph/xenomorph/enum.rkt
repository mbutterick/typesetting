#lang racket/base
(require racket/class "helper.rkt" racket/list)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Enum.coffee
|#

(define xenum%
  (class xenobase%
    (super-new)
    (init-field [(@type type)] [(@values values)])
     
    (unless (xenomorphic-type? @type)
      (raise-argument-error '+xenum "xenomorphic type" @type))
    (unless (list? @values)
      (raise-argument-error '+xenum "list of values" @values))

    (define/augment (xxdecode port parent)
      (define index (send @type xxdecode port parent))
      (or (list-ref @values index) index))

    (define/augment (xxencode val port [parent #f])
      (define index (index-of @values val))
      (unless index
        (raise-argument-error 'xenum-encode "valid option" val))
      (send @type xxencode index port parent))
    
    (define/augment (xxsize [val #f] [parent #f])
      (send @type xxsize val parent))))

(define (+xenum [type-arg #f] [values-arg #f]
                #:type [type-kwarg #f]
                #:values [values-kwarg #f]
                #:pre-encode [pre-proc #f]
                #:post-decode [post-proc #f])
  (define type (or type-arg type-kwarg))
  (define values (or values-arg values-kwarg))
  (new (generate-subclass xenum% pre-proc post-proc) [type type] [values values]))