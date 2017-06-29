#lang restructure/racket
(require "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Bitfield.coffee
|#

(define-subclass Streamcoder (Bitfield type [flags empty])
          
  (define/augment (decode stream . args)
    (for*/fold ([flag-hash (mhash)])
               ([val (in-value (send type decode stream))]
                [(flag i) (in-indexed flags)]
                #:when flag)
      (hash-set! flag-hash flag (bitwise-bit-set? val i))
      flag-hash))

  (define/override (size . args) (send type size))

  (define/augment (encode stream flag-hash [ctx #f])
    (define bitfield-int (for/sum ([(flag i) (in-indexed flags)]
                                   #:when (and flag (hash-ref flag-hash flag)))
                           (expt 2 i)))
    (send type encode stream bitfield-int)))


(test-module
 (require "number.rkt" "stream.rkt")
 (define bfer (+Bitfield uint16be '(bold italic underline #f shadow condensed extended)))
 (define bf (send bfer decode (+DecodeStream #"\0\25")))
 (check-equal? (length (hash-keys bf)) 6) ; omits #f flag
 (check-true (hash-ref bf 'bold))
 (check-true (hash-ref bf 'underline))
 (check-true (hash-ref bf 'shadow))
 (check-false (hash-ref bf 'italic))
 (check-false (hash-ref bf 'condensed))
 (check-false (hash-ref bf 'extended))

 (define os (+EncodeStream))
 (send bfer encode os bf)
 (check-equal? (send os dump) #"\0\25"))