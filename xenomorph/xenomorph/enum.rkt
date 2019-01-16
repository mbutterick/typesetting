#lang racket/base
(require racket/class racket/match "base.rkt" racket/list)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Enum.coffee
|#

(define x:enum%
  (class xenobase%
    (super-new)
    (init-field [(@type type)] [(@values values)])
     
    (unless (xenomorphic-type? @type)
      (raise-argument-error 'x:enum "xenomorphic type" @type))
    (unless (list? @values)
      (raise-argument-error 'x:enum "list of values" @values))

    (define/augment (:decode port parent)
      (define index (send @type :decode port parent))
      (or (list-ref @values index) index))

    (define/augment (:encode val port [parent #f])
      (match (index-of @values val)
        [(? values idx) (send @type :encode idx port parent)]
        [_ (raise-argument-error 'x:enum-encode "valid option" val)]))
    
    (define/augment (:size [val #f] [parent #f])
      (send @type :size val parent))))

(define (x:enum [type-arg #f] [values-arg #f]
                #:type [type-kwarg #f]
                #:values [values-kwarg #f]
                #:pre-encode [pre-proc #f]
                #:post-decode [post-proc #f])
  (define type (or type-arg type-kwarg))
  (define values (or values-arg values-kwarg))
  (new (generate-subclass x:enum% pre-proc post-proc) [type type] [values values]))