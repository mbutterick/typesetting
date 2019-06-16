#lang racket/base
(require "base.rkt"
         "int.rkt"
         racket/class
         racket/dict
         racket/list
         racket/contract
         sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Bitfield.coffee
|#

(define x:bitfield%
  (class x:base%
    (super-new)
    (init-field [(@type type)]
                [(@flags flags)])

    (let ([named-flags (filter values @flags)])
      (unless (= (length named-flags) (length (remove-duplicates named-flags)))
        (raise-argument-error 'x:bitfield% "no duplicates among flag names" named-flags)))

    (when (> (length @flags) (* 8 (send @type x:size)))
      (raise-argument-error 'x:bitfield% (format "~a flags or fewer (~a-byte bitfield)"  (* 8 (send @type x:size)) (send @type x:size)) (length @flags)))

    (define/augment (x:decode port parent)
      (define val (send @type x:decode port))
      (define flag-hash (mhash))
      (for ([(flag idx) (in-indexed @flags)]
            #:when flag)
           (hash-set! flag-hash flag (bitwise-bit-set? val idx)))
      flag-hash)

    (define/augment (x:encode flag-hash port [parent #f])
      (define invalid-flags
        (for/list ([flag (in-hash-keys flag-hash)]
                   #:unless (member flag @flags))
          flag))
      (unless (null? invalid-flags)
        (raise-argument-error 'encode (format "valid flag name ~v" @flags) invalid-flags))
      (define bit-int (for/sum ([(flag idx) (in-indexed @flags)]
                                #:when (and flag (hash-ref flag-hash flag #f)))
                               (arithmetic-shift 1 idx)))
      (send @type x:encode bit-int port))
    
    (define/augment (x:size [val #f] [parent #f])
      (send @type x:size))))

(define (x:bitfield? x) (is-a? x x:bitfield%))
  
(define/contract (x:bitfield
                  [type-arg #f]
                  [flag-arg #f]
                  #:type [type-kwarg uint8]
                  #:flags [flag-kwarg null]
                  #:pre-encode [pre-proc #f]
                  #:post-decode [post-proc #f]
                  #:base-class [base-class x:bitfield%])
  (()
   ((or/c x:int? #false)
    (listof any/c)
    #:type (or/c x:int? #false)
    #:flags (listof any/c)
    #:pre-encode (or/c (any/c . -> . any/c) #false)
    #:post-decode (or/c (any/c . -> . any/c) #false)
    #:base-class (λ (c) (subclass? c x:bitfield%)))
   . ->* .
   x:bitfield?)
  (define type (or type-arg type-kwarg))
  (define flags (or flag-arg flag-kwarg))
  (new (generate-subclass base-class pre-proc post-proc)
       [type type]
       [flags flags]))

(module+ test
  (require rackunit "number.rkt" "base.rkt")
  (define bfer (x:bitfield uint16be '(bold italic underline #f shadow condensed extended)))
  (define bf (decode bfer #"\0\25"))
  (check-equal? (length (hash-keys bf)) 6) ; omits #f flag
  (check-true (hash-ref bf 'bold))
  (check-true (hash-ref bf 'underline))
  (check-true (hash-ref bf 'shadow))
  (check-false (hash-ref bf 'italic))
  (check-false (hash-ref bf 'condensed))
  (check-false (hash-ref bf 'extended))
  (check-equal? (encode bfer bf #f) #"\0\25"))