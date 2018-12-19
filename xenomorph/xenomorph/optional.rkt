#lang racket/base
(require "helper.rkt" racket/class racket/match)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Optional.coffee
|#

(define x:optional%
  (class xenobase%
    (super-new)
    (init-field [(@type type)] [(@condition condition)])

    (unless (xenomorphic-type? @type)
      (raise-argument-error 'x:optional"xenomorphic type" @type))
    
    (define (resolve-condition parent)
      (match @condition
        [(? procedure? proc) (proc parent)]
        [val val]))

    (define/augment (x:decode port parent)
      (when (resolve-condition parent)
        (send @type x:decode port parent)))

    (define/augment (x:encode val port [parent #f])
      (when (resolve-condition parent)
        (send @type x:encode val port parent)))
    
    (define/augment (x:size [val #f] [parent #f])
      (if (resolve-condition parent) (send @type x:size val parent) 0))))

(define no-val (gensym))
(define (x:optional [type-arg #f] [cond-arg no-val]
                    #:type [type-kwarg #f]
                    #:condition [cond-kwarg no-val]
                    #:pre-encode [pre-proc #f]
                    #:post-decode [post-proc #f])
  (define type (or type-arg type-kwarg))
  (define condition (cond
                      [(and (eq? cond-arg no-val) (eq? cond-kwarg no-val)) #true]
                      [(not (eq? cond-arg no-val)) cond-arg]
                      [(not (eq? cond-kwarg no-val)) cond-kwarg]))
  (new (generate-subclass x:optional% pre-proc post-proc) [type type] [condition condition]))
