#lang racket/base
(require racket/class "base.rkt" "util.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Reserved.coffee
|#

(define x:reserved%
  (class xenobase%
    (super-new)
    (init-field [(@type type)] [(@count count)])

    (unless (xenomorphic-type? @type)
      (raise-argument-error '+xoptional "xenomorphic type" @type))

    (define/augment (decode port parent)
      (pos port (+ (pos port) (size #f parent)))
      (void))

    (define/augment (encode val port [parent #f])
      (make-bytes (size val parent) 0))
    
    (define/augment (size [val #f] [parent #f])
      (* (send @type size) (resolve-length @count #f parent)))))

(define (x:reserved [type-arg #f] [count-arg #f]
                    #:type [type-kwarg #f]
                    #:count [count-kwarg #f]
                    #:pre-encode [pre-proc #f]
                    #:post-decode [post-proc #f])
  (define type (or type-arg type-kwarg))
  (define count (or count-arg count-kwarg 1))
  (new (generate-subclass x:reserved% pre-proc post-proc) [type type] [count count]))