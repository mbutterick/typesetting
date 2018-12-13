#lang racket/base
(require "helper.rkt" racket/dict sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Bitfield.coffee
|#

(define/post-decode (xbitfield-decode xb [port-arg (current-input-port)] #:parent [parent #f])
  (define port (->input-port port-arg))
  (parameterize ([current-input-port port])
    (define flag-hash (mhasheq))
    (define val (decode (xbitfield-type xb)))
    (for ([(flag i) (in-indexed (xbitfield-flags xb))]
          #:when flag)
      (hash-set! flag-hash flag (bitwise-bit-set? val i)))
    flag-hash))

(define/pre-encode (xbitfield-encode xb flag-hash [port-arg (current-output-port)] #:parent [parent #f])
  (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
  (parameterize ([current-output-port port])
    (define bit-int (for/sum ([(flag i) (in-indexed (xbitfield-flags xb))]
                              #:when (and flag (dict-ref flag-hash flag #f)))
                      (arithmetic-shift 1 i)))
    (encode (xbitfield-type xb) bit-int)
    (unless port-arg (get-output-bytes port))))

(define (xbitfield-size xb [val #f] #:parent [parent #f])
  (size (xbitfield-type xb)))

(struct xbitfield xbase (type flags) #:transparent
  #:methods gen:xenomorphic
  [(define decode xbitfield-decode)
   (define encode xbitfield-encode)
   (define size xbitfield-size)])

(define (+xbitfield [type-arg #f] [flag-arg #f]
                    #:type [type-kwarg #f] #:flags [flag-kwarg #f])
  (define type (or type-arg type-kwarg))
  (define flags (or flag-arg flag-kwarg null))
  (unless (andmap (λ (f) (or (symbol? f) (not f))) flags)
    (raise-argument-error '+xbitfield "list of symbols" flags))
  (xbitfield type flags))

(module+ test
  (require rackunit "number.rkt")
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