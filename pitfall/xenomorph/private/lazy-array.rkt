#lang reader (submod "racket.rkt" reader)
(require "utils.rkt" "array.rkt" "number.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/LazyArray.coffee
|#

(define (get o i) (send o get i))

(define-subclass object% (InnerLazyArray type [len #f] [stream #f] [ctx #f])
  (unless stream (raise-argument-error 'LazyArray "stream" stream))
  (define starting-pos (· stream pos))
  (define item-cache (mhasheqv)) ;  integer-keyed hash, rather than list

  (define/public-final (get index)
    (unless (<= 0 index (sub1 len))
      #;(raise-argument-error 'LazyArray:get (format "index in range 0 to ~a" len) index)
      (void))
    (ref! item-cache index (λ ()
                             (define orig-pos (· stream pos))
                             (send stream pos (+ starting-pos (* (send type size #f ctx) index)))
                             (define new-item (send type decode stream ctx))
                             (send stream pos orig-pos)
                             new-item)))

  (define/public-final (to-list)
    (for/list ([i (in-range len)])
      (get i))))


(define-subclass ArrayT (LazyArray)
  (inherit-field len type)
  
  (define/override (decode stream [parent #f])
    (define pos (· stream pos)) ; ! placement matters. `resolve-length` will change `pos`
    (define decoded-len (resolve-length len stream parent))
    (let ([parent (if (NumberT? len)
                      (mhasheq 'parent parent
                               '_startOffset pos
                               '_currentOffset 0
                               '_length len)
                      parent)])
      (define res (+InnerLazyArray type decoded-len stream parent))
      (send stream pos (+ (· stream pos) (* decoded-len (send type size #f parent))))
      res))

  (define/override (size [val #f] [ctx #f])
    (super size (if (InnerLazyArray? val)
                    (send val to-list)
                    val) ctx))

  (define/override (encode stream val [ctx #f])
    (super encode stream (if (InnerLazyArray? val)
                             (send val to-list)
                             val) ctx)))    

(test-module
 (require "stream.rkt")
 (define bstr #"ABCD1234")
 (define ds (+DecodeStream bstr))
 (define la (+LazyArray uint8 4))
 (define ila (send la decode ds))
 (check-equal? (send ds pos) 4)
 (check-equal? (send ila get 1) 66)
 (check-equal? (send ila get 3) 68)
 (check-equal? (send ds pos) 4)
 (check-equal? (send ila to-list) '(65 66 67 68))

 (define la2 (+LazyArray int16be (λ (t) 4)))
 (define es (+EncodeStream))
 (send la2 encode es '(1 2 3 4))
 (check-equal? (send es dump) #"\0\1\0\2\0\3\0\4")
 (check-equal? (send (send la2 decode (+DecodeStream #"\0\1\0\2\0\3\0\4")) to-list) '(1 2 3 4)))

