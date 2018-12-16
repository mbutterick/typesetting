#lang racket/base
(require "helper.rkt" racket/class racket/dict sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Bitfield.coffee
|#

#;(define/post-decode (xbitfield-decode xb [port-arg (current-input-port)] #:parent [parent #f])
    (define port (->input-port port-arg))
    (parameterize ([current-input-port port])
      ))

#;(define/pre-encode (xbitfield-encode xb flag-hash [port-arg (current-output-port)] #:parent [parent #f])
    (define port (if (output-port? port-arg) port-arg (open-output-bytes)))
    (parameterize ([current-output-port port])
      
      (unless port-arg (get-output-bytes port))))

#;(define (xbitfield-size xb [val #f] #:parent [parent #f])
    )

#;(struct xbitfield xbase (type flags) #:transparent
    #:methods gen:xenomorphic
    [(define decode xbitfield-decode)
     (define xdecode xbitfield-decode)
     (define encode xbitfield-encode)
     (define size xbitfield-size)])

(define xbitfield%
  (class xenobase%
    (super-new)
    (init-field type flags)

    (define flag-hash (mhasheq))

    (define/augment (xxdecode port parent)
      (define val (send type xxdecode port))
      (for ([(flag i) (in-indexed flags)]
            #:when flag)
           (hash-set! flag-hash flag (bitwise-bit-set? val i)))
      flag-hash)

    (define/augment (xxencode array port [parent #f])
      (define bit-int (for/sum ([(flag i) (in-indexed flags)]
                                #:when (and flag (dict-ref flag-hash flag #f)))
                               (arithmetic-shift 1 i)))
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