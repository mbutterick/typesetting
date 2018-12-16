#lang debug racket/base
(require "helper.rkt" racket/class)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Optional.coffee
|#

(define xoptional%
  (class xenobase%
    (super-new)
    (init-field [(@type type)] [(@condition condition)])

    (unless (xenomorphic-type? @type)
      (raise-argument-error '+xoptional"xenomorphic type" @type))
    
    (define (resolve-condition parent)
      (define maybe-proc @condition)
      (if (procedure? maybe-proc) (maybe-proc parent) maybe-proc))

    (define/augment (xxdecode port parent)
      (when (resolve-condition parent)
        (send @type xxdecode port parent)))

    (define/augment (xxencode val port [parent #f])
      (when (resolve-condition parent)
        (send @type xxencode val port parent)))
    
    (define/augment (xxsize [val #f] [parent #f])
      (if (resolve-condition parent) (send @type xxsize val parent) 0))))


(define no-val (gensym))
(define (+xoptional [type-arg #f] [cond-arg no-val]
                    #:type [type-kwarg #f]
                    #:condition [cond-kwarg no-val]
                    #:subclass [class xoptional%])
  (define type (or type-arg type-kwarg))
  (define condition (cond
                      [(and (eq? cond-arg no-val) (eq? cond-kwarg no-val)) #true]
                      [(not (eq? cond-arg no-val)) cond-arg]
                      [(not (eq? cond-kwarg no-val)) cond-kwarg]))
  (new class [type type] [condition condition]))
