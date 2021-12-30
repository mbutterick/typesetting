#lang racket/base
(require "base.rkt"
         racket/class
         racket/match
         racket/contract)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Optional.coffee
|#

(define x:optional%
  (class x:base%
    (super-new)
    (init-field [(@type type)] [(@condition condition)])

    (unless (xenomorphic? @type)
      (raise-argument-error 'x:optional "xenomorphic type" @type))
    
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

(define (x:optional? x) (is-a? x x:optional%))

(define/contract (x:optional
                  [type-arg #f]
                  [cond-arg no-val]
                    #:type [type-kwarg #f]
                    #:condition [cond-kwarg no-val]
                    #:pre-encode [pre-proc #f]
                    #:post-decode [post-proc #f]
                    #:base-class [base-class x:optional%])
  (()
   ((or/c xenomorphic? #false)
    any/c
    #:type (or/c xenomorphic? #false)
    #:condition any/c
    #:pre-encode (or/c (any/c . -> . any/c) #false)
    #:post-decode (or/c (any/c . -> . any/c) #false)
    #:base-class (λ (c) (subclass? c x:optional%)))
   . ->* .
   x:optional?)
  (define type (or type-arg type-kwarg))
  (unless (xenomorphic? type)
    (raise-argument-error 'x:optional "xenomorphic type" type))
  (define condition (cond
                      [(and (eq? cond-arg no-val) (eq? cond-kwarg no-val)) #true]
                      [(not (eq? cond-arg no-val)) cond-arg]
                      [(not (eq? cond-kwarg no-val)) cond-kwarg]))
  (new (generate-subclass base-class pre-proc post-proc) [type type] [condition condition]))
