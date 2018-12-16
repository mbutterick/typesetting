#lang racket/base
(require "helper.rkt" racket/class racket/dict sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Bitfield.coffee
|#

(define xbitfield%
  (class xenobase%
    (super-new)
    (init-field type flags)

    (define/augment (xxdecode port parent)
      (define flag-hash (mhasheq))
      (define val (send type xxdecode port))
      (for ([(flag idx) (in-indexed flags)]
            #:when flag)
        (hash-set! flag-hash flag (bitwise-bit-set? val idx)))
      flag-hash)

    (define/augment (xxencode flag-hash port [parent #f])
      (define bit-int (for/sum ([(flag idx) (in-indexed flags)]
                                #:when (and flag (dict-ref flag-hash flag #f)))
                        (arithmetic-shift 1 idx)))
      (send type xxencode bit-int port))
    
    (define/augment (xxsize [val #f] [parent #f])
      (send type xxsize))))
  
(define (+xbitfield [type-arg #f] [flag-arg #f]
                    #:type [type-kwarg #f]
                    #:flags [flag-kwarg #f]
                    #:subclass [class xbitfield%])
  (define type (or type-arg type-kwarg))
  (define flags (or flag-arg flag-kwarg null))
  (unless (andmap (λ (f) (or (symbol? f) (not f))) flags)
    (raise-argument-error '+xbitfield "list of symbols" flags))
  (new class [type type] [flags flags]))

(module+ test
  (require rackunit "number.rkt" "generic.rkt")
  (define bfer (+xbitfield uint16be '(bold italic underline #f shadow condensed extended)))
  (define bf (decode bfer #"\0\25"))
  (check-equal? (length (dict-keys bf)) 6) ; omits #f flag
  (check-true (dict-ref bf 'bold))
  (check-true (dict-ref bf 'underline))
  (check-true (dict-ref bf 'shadow))
  (check-false (dict-ref bf 'italic))
  (check-false (dict-ref bf 'condensed))
  (check-false (dict-ref bf 'extended))
  (check-equal? (encode bfer bf #f) #"\0\25"))