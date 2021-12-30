#lang racket/base
(require racket/class
         racket/contract
         "base.rkt"
         "util.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Reserved.coffee
|#

(define x:reserved%
  (class x:base%
    (super-new)
    (init-field [(@type type)] [(@count count)])

    (unless (xenomorphic? @type)
      (raise-argument-error 'x:reserved "xenomorphic type" @type))

    (define/augment (x:decode port parent)
      (pos port (+ (pos port) (x:size #f parent)))
      (void))

    (define/augment (x:encode val port [parent #f])
      (make-bytes (x:size val parent) 0))
    
    (define/augment (x:size [val #f] [parent #f])
      (* (send @type x:size) (resolve-length @count #f parent)))))

(define (x:reserved? x) (is-a? x x:reserved%))

(define/contract (x:reserved [type-arg #f]
                             [count-arg #f]
                             #:type [type-kwarg #f]
                             #:count [count-kwarg 1]
                             #:pre-encode [pre-proc #f]
                             #:post-decode [post-proc #f]
                             #:base-class [base-class x:reserved%])
  (()
   ((or/c xenomorphic? #false)
    (or/c exact-positive-integer? #false)
    #:type (or/c xenomorphic? #false)
    #:count exact-positive-integer?
    #:pre-encode (or/c (any/c . -> . any/c) #false)
    #:post-decode (or/c (any/c . -> . any/c) #false)
    #:base-class (λ (c) (subclass? c x:reserved%)))
   . ->* .
   x:reserved?)
  (define type (or type-arg type-kwarg))
    (unless (xenomorphic? type)
    (raise-argument-error 'x:reserved "xenomorphic type" type))
  (define count (or count-arg count-kwarg))
  (new (generate-subclass base-class pre-proc post-proc)
       [type type]
       [count count]))