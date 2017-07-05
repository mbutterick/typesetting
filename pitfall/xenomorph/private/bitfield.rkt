#lang reader (submod "racket.rkt" reader)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Bitfield.coffee
|#

(define-subclass Streamcoder (Bitfield type [flags empty])
  (unless (andmap (λ (f) (or (key? f) (not f))) flags)
    (raise-argument-error 'Bitfield "list of keys" flags))
          
  (define/augment (decode stream . _)
    (define flag-hash (mhasheq))
    (for* ([val (in-value (send type decode stream))]
           [(flag i) (in-indexed flags)]
           #:when flag)
      (hash-set! flag-hash flag (bitwise-bit-set? val i)))
    flag-hash)

  (define/augment (size . _) (send type size))

  (define/augment (encode port flag-hash [ctx #f])
    (define bit-int (for/sum ([(flag i) (in-indexed flags)]
                                       #:when (and flag (ref flag-hash flag)))
                               (arithmetic-shift 1 i)))
    (send type encode port bit-int))

  (define/override (get-class-name) 'Bitfield))


(test-module
 (require "number.rkt")
 (define bfer (+Bitfield uint16be '(bold italic underline #f shadow condensed extended)))
 (define bf (send bfer decode #"\0\25"))
 (check-equal? (length (ref-keys bf)) 6) ; omits #f flag
 (check-true (ref bf 'bold))
 (check-true (ref bf 'underline))
 (check-true (ref bf 'shadow))
 (check-false (ref bf 'italic))
 (check-false (ref bf 'condensed))
 (check-false (ref bf 'extended))
 (check-equal? (encode bfer bf #f) #"\0\25"))