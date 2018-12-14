#lang racket/base
(require "helper.rkt" "util.rkt" "number.rkt" "array.rkt" racket/stream racket/dict sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/LazyArray.coffee
|#
  
(define (xlazy-array-decode xla [port-arg (current-input-port)] #:parent [parent #f])
  (define port (->input-port port-arg))
  (parameterize ([current-input-port port])
    (define starting-pos (pos port)) ; ! placement matters. `resolve-length` will change `pos`
    (define decoded-len (resolve-length (xarray-base-len xla) #:parent parent))
    (let ([parent (if (xint? (xarray-base-len xla))
                      (mhasheq 'parent parent
                               '_startOffset starting-pos
                               '_currentOffset 0
                               '_length (xarray-base-len xla))
                      parent)])
      (define starting-pos (pos port))
      (define type (xarray-base-type xla))
      (begin0
        (for/stream ([index (in-range decoded-len)])
          (define orig-pos (pos port))
          (pos port (+ starting-pos (* (size type #f #:parent parent) index)))
          ;; use explicit `port` arg below because this evaluation is delayed
          (begin0
            (post-decode xla (xdecode type port #:parent parent))
            (pos port orig-pos)))
        (pos port (+ (pos port) (* decoded-len (size (xarray-base-type xla) #f #:parent parent))))))))

(define (xlazy-array-encode xla val [port-arg (current-output-port)] #:parent [parent #f])
  (xarray-encode xla (if (stream? val) (stream->list val) val) port-arg #:parent parent))

(define (xlazy-array-size xla [val #f] #:parent [parent #f])
  (xarray-size xla (if (stream? val) (stream->list val) val) #:parent parent))

;; xarray-base holds type and len fields
(struct xlazy-array xarray-base () #:transparent
  #:methods gen:xenomorphic
  [(define decode xlazy-array-decode)
   (define xdecode xlazy-array-decode)
   (define encode xlazy-array-encode)
   (define size xlazy-array-size)])

(define (+xlazy-array [type-arg #f] [len-arg #f]
                      #:type [type-kwarg #f] #:length [len-kwarg #f])
  (define type (or type-arg type-kwarg))
  (define len (or len-arg len-kwarg))
  (unless (xenomorphic? type)
    (raise-argument-error '+xarray "xenomorphic type" type))
  (unless (length-resolvable? len)
    (raise-argument-error '+xarray "length-resolvable?" len))
  (xlazy-array type len))

(module+ test
  (require rackunit "number.rkt")
  (define bstr #"ABCD1234")
  (define ds (open-input-bytes bstr))
  (define la (+xlazy-array uint8 4))
  (define ila (decode la ds))
  (check-equal? (pos ds) 4)
  (check-equal? (stream-ref ila 1) 66)
  (check-equal? (stream-ref ila 3) 68)
  (check-equal? (pos ds) 4)
  (check-equal? (stream->list ila) '(65 66 67 68))
  (define la2 (+xlazy-array int16be (λ (t) 4))) 
  (check-equal? (encode la2 '(1 2 3 4) #f) #"\0\1\0\2\0\3\0\4")
  (check-equal? (stream->list (decode la2 (open-input-bytes #"\0\1\0\2\0\3\0\4"))) '(1 2 3 4)))