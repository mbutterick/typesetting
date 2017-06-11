#lang restructure/racket
(require "streamcoder.rkt")
(provide RBitfield)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Bitfield.coffee
|#

(define-subclass RStreamcoder (RBitfield type [flags empty])
          
  (define/augment (decode stream [parent #f])
    (for*/fold ([res (mhash)])
               ([val (in-value (send type decode stream))]
                [(flag i) (in-indexed flags)])
      (hash-set! res flag (bitwise-bit-set? val i))
      res))

  (define/override (size . args) (send type size))

  (define/augment (encode stream flag-hash)
    (send type encode stream (for/sum ([(flag i) (in-indexed flags)]
                                       #:when (hash-ref flag-hash flag))
                               (expt 2 i)))))


(test-module
 (require "number.rkt" "decodestream.rkt" "encodestream.rkt")
 (define bfer (make-object RBitfield uint16be '(bold italic underline outline shadow condensed extended)))
 (define bf (send bfer decode (make-object RDecodeStream #"\0\25")))
 (check-true (hash-ref bf 'bold))
 (check-true (hash-ref bf 'underline))
 (check-true (hash-ref bf 'shadow))
 (check-false (hash-ref bf 'italic))
 (check-false (hash-ref bf 'outline))
 (check-false (hash-ref bf 'condensed))
 (check-false (hash-ref bf 'extended))

 (define os (make-object REncodeStream))
 (send bfer encode os bf)
 (check-equal? (send os dump) #"\0\25"))