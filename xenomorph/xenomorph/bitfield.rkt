#lang racket/base
(require "base.rkt" racket/class racket/dict sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Bitfield.coffee
|#

(define x:bitfield%
  (class xenobase%
    (super-new)
    (init-field [(@type type)][(@flags flags)])
    (unless (andmap (λ (f) (or (symbol? f) (not f))) @flags)
      (raise-argument-error '+xbitfield "list of symbols" @flags))

    (define/augment (decode port parent)
      (define val (send @type decode port))
      (define flag-hash (mhasheq))
      (for ([(flag idx) (in-indexed @flags)]
            #:when flag)
        (hash-set! flag-hash flag (bitwise-bit-set? val idx)))
      flag-hash)

    (define/augment (encode flag-hash port [parent #f])
      (define bit-int (for/sum ([(flag idx) (in-indexed @flags)]
                                #:when (and flag (hash-ref flag-hash flag #f)))
                        (arithmetic-shift 1 idx)))
      (send @type encode bit-int port))
    
    (define/augment (size [val #f] [parent #f])
      (send @type size))))
  
(define (x:bitfield [type-arg #f] [flag-arg #f]
                    #:type [type-kwarg #f]
                    #:flags [flag-kwarg #f]
                    #:pre-encode [pre-proc #f]
                    #:post-decode [post-proc #f])
  (define type (or type-arg type-kwarg))
  (define flags (or flag-arg flag-kwarg null))
  (new (generate-subclass x:bitfield% pre-proc post-proc) [type type] [flags flags]))

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