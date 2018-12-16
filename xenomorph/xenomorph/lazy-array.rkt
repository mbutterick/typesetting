#lang debug racket/base
(require racket/class
         "helper.rkt" "util.rkt" "number.rkt" "array.rkt" racket/stream sugar/unstable/dict)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/LazyArray.coffee
|#
 
(define xlazy-array%
  (class xarray%
    (super-new)
    (inherit-field type len)

    (define/override (xxdecode port parent)
      (define starting-pos (pos port)) ; ! placement matters. `resolve-length` will change `pos`
      (define decoded-len (resolve-length len #:parent parent))
      (let ([parent (if (xint? len)
                        (mhasheq 'parent parent
                                 '_startOffset starting-pos
                                 '_currentOffset 0
                                 '_length len)
                        parent)])
        (define starting-pos (pos port))
        (begin0
          (for/stream ([index (in-range decoded-len)])
            (define orig-pos (pos port))
            (pos port (+ starting-pos (* (send type xxsize #f parent) index)))
            (begin0
              (send type xxdecode port parent)
              (pos port orig-pos)))
          (pos port (+ (pos port) (* decoded-len (send type xxsize #f parent)))))))

    (define/override (xxencode val port [parent #f])
      (super xxencode (if (stream? val) (stream->list val) val) port parent))
    
    (define/override (xxsize [val #f] [parent #f])
      (super xxsize (if (stream? val) (stream->list val) val) parent))))

(define (+xlazy-array [type-arg #f] [len-arg #f]
                      #:type [type-kwarg #f]
                      #:length [len-kwarg #f]
                      #:subclass [class xlazy-array%])
  (define type (or type-arg type-kwarg))
  (define len (or len-arg len-kwarg))
  (new class [type type]
       [len len]
       [length-type 'count]))

(module+ test
  (require rackunit "number.rkt" "generic.rkt")
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