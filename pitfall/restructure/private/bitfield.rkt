#lang reader (submod "racket.rkt" reader)
(require "stream.rkt")
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

  (define/override (size . _) (send type size))

  (define/augment (encode stream flag-hash [ctx #f])
    (define bitfield-integer (for/sum ([(flag i) (in-indexed flags)]
                                       #:when (and flag (ref flag-hash flag)))
                               (arithmetic-shift 1 i)))
    (send type encode stream bitfield-integer)))


(test-module
 (require "number.rkt" "stream.rkt")
 (define bfer (+Bitfield uint16be '(bold italic underline #f shadow condensed extended)))
 (define bf (send bfer decode (+DecodeStream #"\0\25")))
 (check-equal? (length (ref-keys bf)) 6) ; omits #f flag
 (check-true (ref bf 'bold))
 (check-true (ref bf 'underline))
 (check-true (ref bf 'shadow))
 (check-false (ref bf 'italic))
 (check-false (ref bf 'condensed))
 (check-false (ref bf 'extended))

 (define os (+EncodeStream))
 (send bfer encode os bf)
 (check-equal? (send os dump) #"\0\25"))