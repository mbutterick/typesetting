#lang debug racket/base
(require racket/class
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
      (unless (or (vector? val) (list? val))
        (raise-argument-error 'x:vector-encode "vector or list" val))
      (if (vector? val) (vector->list val) val))
    
    (define/override (post-decode val) (list->vector val))))

(define (x:vector [type-arg #f] [len-arg #f] [length-type-arg 'count]
                 #:type [type-kwarg #f]
                 #:length [len-kwarg #f]
                 #:count-bytes [count-bytes? #f]
                 #:pre-encode [pre-proc #f]
                 #:post-decode [post-proc #f]
                 #:base-class [base-class x:vector%])
  (new (generate-subclass base-class pre-proc post-proc) [type (or type-arg type-kwarg)]
       [len (or len-arg len-kwarg)]
       [count-bytes? count-bytes?]))

(define (x:vector? x) (is-a? x x:vector%))

(module+ test
  (require rackunit)
  (check-equal? (decode (x:vector uint16be 3) #"ABCDEF") '#(16706 17220 17734))
  (check-equal? (encode (x:vector uint16be 3) '(16706 17220 17734) #f) #"ABCDEF")
  (check-equal? (encode (x:vector uint16be 3) '#(16706 17220 17734) #f) #"ABCDEF")
  (check-equal? (size (x:vector uint16be) '#(1 2 3)) 6)
  (check-equal? (size (x:vector doublebe) '#(1 2 3 4 5)) 40))