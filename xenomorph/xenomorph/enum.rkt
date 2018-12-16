#lang racket/base
(require racket/class "helper.rkt" racket/list)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Enum.coffee
|#

#;(define/post-decode (xenum-decode xe [port-arg (current-input-port)] #:parent [parent #f])
    (define port (->input-port port-arg))
    (parameterize ([current-input-port port])
      ))

#;(define (xenum-size xe [val #f] #:parent [parent #f])
    )

#;(define/pre-encode (xenum-encode xe val [port-arg (current-output-port)] #:parent [parent #f])
    (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
    (parameterize ([current-output-port port])

      (unless port-arg (get-output-bytes port))))

#;(struct xenum xbase (type options) #:transparent
    #:methods gen:xenomorphic
    [(define decode xenum-decode)
     (define xdecode xenum-decode)
     (define encode xenum-encode)
     (define size xenum-size)])

(define xenum%
  (class xenobase%
    (super-new)
    (init-field type values)

    (define/augment (xxdecode port parent)
      (define index (send type xxdecode port parent))
      (or (list-ref values index) index))

    (define/augment (xxencode val port [parent #f])
      (define index (index-of values val))
      (unless index
        (raise-argument-error 'xenum-encode "valid option" val))
      (send type xxencode index port parent))
    
    (define/augment (xxsize [val #f] [parent #f])
      (send type xxsize))))

(define (+xenum [type-arg #f] [values-arg #f]
                #:type [type-kwarg #f]
                #:values [values-kwarg #f]
                #:subclass [class xenum%])
  (define type (or type-arg type-kwarg))
  (unless (xenomorphic-type? type)
    (raise-argument-error '+xenum "xenomorphic type" type))
  (define values (or values-arg values-kwarg))
  (unless (list? values)
    (raise-argument-error '+xenum "list of values" values))
  (new class [type type] [values values]))