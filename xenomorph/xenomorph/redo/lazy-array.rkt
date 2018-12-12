#lang racket/base
(require "helper.rkt" "util.rkt" "number.rkt" "array.rkt" racket/dict sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/LazyArray.coffee
|#
(define (get ila index)
  (unless (<= 0 index (sub1 (inner-lazy-array-len ila)))
    (raise-argument-error 'LazyArray:get (format "index in range 0 to ~a" (sub1 (inner-lazy-array-len ila))) index))
  (dict-ref! (inner-lazy-array-item-cache ila) index (λ ()
                                                       (define port (inner-lazy-array-port ila))
                                                       (define orig-pos (pos port))
                                                       (pos port (+ (inner-lazy-array-starting-pos ila)
                                                                    (* (size (inner-lazy-array-type ila) #f (inner-lazy-array-ctx ila)) index)))
                                                       (define new-item (decode (inner-lazy-array-type ila) port #:parent (inner-lazy-array-ctx ila)))
                                                       (pos port orig-pos)
                                                       new-item)))

(define (to-list ila)
  (for/list ([i (in-range (inner-lazy-array-len ila))])
    (get ila i)))

(define (xlazy-array->list ila) (to-list ila))

(struct inner-lazy-array (type len port ctx starting-pos item-cache) #:transparent)

(define (+inner-lazy-array type [len #f] [port-in #f] [ctx #f])
  (define port (->input-port port-in))
  (define starting-pos (pos port))
  (inner-lazy-array type len port ctx starting-pos (mhasheqv)))
  

(define (xlazy-array-decode xla [port-arg (current-input-port)] #:parent [parent #f])
  (define port (->input-port port-arg))
  (define starting-pos (pos port)) ; ! placement matters. `resolve-length` will change `pos`
  (define decoded-len (resolve-length (xarray-base-len xla) port parent))
  (let ([parent (if (xint? (xarray-base-len xla))
                    (mhasheq 'parent parent
                             '_startOffset starting-pos
                             '_currentOffset 0
                             '_length (xarray-base-len xla))
                    parent)])
    (define res (+inner-lazy-array (xarray-base-type xla) decoded-len port parent))
    (pos port (+ (pos port) (* decoded-len (size (xarray-base-type xla) #f parent))))
    res))

(define (xlazy-array-encode xla val [port-arg (current-output-port)] #:parent [parent #f])
  (xarray-encode xla (if (inner-lazy-array? val) (to-list val) val) port-arg #:parent parent))

(define (xlazy-array-size xla [val #f] [ctx #f])
  (xarray-size xla (if (inner-lazy-array? val) (to-list val) val) ctx))

;; xarray-base holds type and len fields
(struct xlazy-array xarray-base () #:transparent
  #:methods gen:xenomorphic
  [(define decode xlazy-array-decode)
   (define encode xlazy-array-encode)
   (define size xlazy-array-size)])

(define (+xlazy-array type [len #f])
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
  (check-equal? (get ila 1) 66)
  (check-equal? (get ila 3) 68)
  (check-equal? (pos ds) 4)
  (check-equal? (xlazy-array->list ila) '(65 66 67 68))
  (define la2 (+xlazy-array int16be (λ (t) 4))) 
  (check-equal? (encode la2 '(1 2 3 4) #f) #"\0\1\0\2\0\3\0\4")
  (check-equal? (to-list (decode la2 (open-input-bytes #"\0\1\0\2\0\3\0\4"))) '(1 2 3 4)))