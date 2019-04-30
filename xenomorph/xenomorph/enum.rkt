#lang racket/base
(require racket/class
         racket/match
         "base.rkt"
         "int.rkt"
         racket/contract
         racket/list)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Enum.coffee
|#

(define x:enum%
  (class x:base%
    (super-new)
    (init-field [(@type type)] [(@values values)])
     
    (unless (x:int? @type)
      (raise-argument-error 'x:enum "xenomorphic integer type" @type))
    
    (unless (list? @values)
      (raise-argument-error 'x:enum "list of values" @values))

    (define/augment (x:decode port parent)
      (define index (send @type x:decode port parent))
      (if (< index (length @values)) 
          (or (list-ref @values index) index)
          index))

    (define/augment (x:encode val port [parent #f])
      (match (index-of @values val)
        [(? values idx) (send @type x:encode idx port parent)]
        [_ (raise-argument-error 'x:enum-encode "valid option" val)]))
    
    (define/augment (x:size [val #f] [parent #f])
      (send @type x:size val parent))))

(define (x:enum? x) (is-a? x x:enum%))

(define/contract (x:enum [type-arg #f]
                         [values-arg #f]
                         #:type [type-kwarg #f]
                         #:values [values-kwarg #f]
                         #:pre-encode [pre-proc #f]
                         #:post-decode [post-proc #f]
                         #:base-class [base-class x:enum%])
  (()
   ((or/c x:int? #false)
    (listof any/c)
    #:type (or/c x:int? #false)
    #:values (listof any/c)
    #:pre-encode (or/c (any/c . -> . any/c) #false)
    #:post-decode (or/c (any/c . -> . any/c) #false)
    #:base-class (λ (c) (subclass? c x:enum%)))
   . ->* .
   x:enum?)
  (define type (or type-arg type-kwarg))
  (define values (or values-arg values-kwarg))
  (new (generate-subclass base-class pre-proc post-proc) [type type] [values values]))