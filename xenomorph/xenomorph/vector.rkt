#lang debug racket/base
(require racket/class
         racket/contract
         racket/match
         racket/sequence
         "base.rkt"
         "number.rkt"
         "util.rkt"
         "list.rkt"
         sugar/unstable/dict)
(provide (all-defined-out))

(define x:vector%
  (class x:list%
    (super-new)

    (define/override (pre-encode val)
      (unless (or (vector? val) (sequence? val))
        (raise-argument-error 'encode "vector or sequence" val))
      (if (vector? val) (vector->list val) val))
    
    (define/override (post-decode val) (list->vector val))))

(define (x:vector? x) (is-a? x x:vector%))

(define/contract (x:vector
                  [type-arg #f]
                  [len-arg #f]
                  #:type [type-kwarg #f]
                  #:length [len-kwarg #f]
                  #:count-bytes [count-bytes? #f]
                  #:pre-encode [pre-proc #f]
                  #:post-decode [post-proc #f]
                  #:base-class [base-class x:vector%])
  (()
   ((or/c xenomorphic? #false)
    (or/c length-resolvable? #false)
    #:type (or/c xenomorphic? #false)
    #:length (or/c length-resolvable? #false)
    #:count-bytes boolean?
    #:pre-encode (or/c (any/c . -> . any/c) #false)
    #:post-decode (or/c (any/c . -> . any/c) #false)
    #:base-class (λ (c) (subclass? c x:vector%)))
   . ->* .
   x:vector?)
  (define type (or type-arg type-kwarg))
  (unless (xenomorphic? type)
    (raise-argument-error 'x:vector "xenomorphic type" type))
  (define len (or len-arg len-kwarg))
  (unless (length-resolvable? len)
    (raise-argument-error 'x:vector "resolvable length" len))
  (new (generate-subclass base-class pre-proc post-proc)
       [type type]
       [len len]
       [count-bytes? count-bytes?]))

(module+ test
  (require rackunit)
  (check-equal? (decode (x:vector uint16be 3) #"ABCDEF") '#(16706 17220 17734))
  (check-equal? (encode (x:vector uint16be 3) '(16706 17220 17734) #f) #"ABCDEF")
  (check-equal? (encode (x:vector uint16be 3) '#(16706 17220 17734) #f) #"ABCDEF")
  (check-equal? (send (x:vector uint16be) x:size '#(1 2 3)) 6)
  (check-equal? (send (x:vector doublebe) x:size '#(1 2 3 4 5)) 40))