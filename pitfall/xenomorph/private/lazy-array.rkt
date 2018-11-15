#lang racket/base
(require "racket.rkt")
(require "utils.rkt" "array.rkt" "number.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/LazyArray.coffee
|#

(define (get o i) (send o get i))
(define (LazyArray->list o) (send o to-list))

(define-subclass object% (InnerLazyArray type [len #f] [port-in #f] [ctx #f])
  (field ([port port] (cond
                        [(bytes? port-in) (open-input-bytes port-in)]
                        [(port? port-in) port-in]
                        [else (raise-argument-error 'LazyArray "port" port)])))
  (define starting-pos (pos port))
  (define item-cache (mhasheqv)) ;  integer-keyed hash, rather than list
  

  (define/public-final (get index)
    (unless (<= 0 index (sub1 len))
      (raise-argument-error 'LazyArray:get (format "index in range 0 to ~a" (sub1 len)) index))
    (ref! item-cache index (λ ()
                             (define orig-pos (pos port))
                             (pos port (+ starting-pos (* (send type size #f ctx) index)))
                             (define new-item (send type decode port ctx))
                             (pos port orig-pos)
                             new-item)))

  (define/public-final (to-list)
    (for/list ([i (in-range len)])
              (get i))))


(define-subclass ArrayT (LazyArray)
  (inherit-field len type)
  
  (define/override (decode port [parent #f])
    (define starting-pos (pos port)) ; ! placement matters. `resolve-length` will change `pos`
    (define decoded-len (resolve-length len port parent))
    (let ([parent (if (NumberT? len)
                      (mhasheq 'parent parent
                               '_startOffset starting-pos
                               '_currentOffset 0
                               '_length len)
                      parent)])
      (define res (+InnerLazyArray type decoded-len port parent))
      (pos port (+ (pos port) (* decoded-len (send type size #f parent))))
      res))

  (define/override (size [val #f] [ctx #f])
    (super size (if (InnerLazyArray? val)
                    (send val to-list)
                    val) ctx))

  (define/override (encode port val [ctx #f])
    (super encode port (if (InnerLazyArray? val)
                           (send val to-list)
                           val) ctx)))    

(test-module
 (define bstr #"ABCD1234")
 (define ds (open-input-bytes bstr))
 (define la (+LazyArray uint8 4))
 (define ila (decode la ds))
 (check-equal? (pos ds) 4)
 (check-equal? (get ila 1) 66)
 (check-equal? (get ila 3) 68)
 (check-equal? (pos ds) 4)
 (check-equal? (LazyArray->list ila) '(65 66 67 68))
 (define la2 (+LazyArray int16be (λ (t) 4))) 
 (check-equal? (encode la2 '(1 2 3 4) #f) #"\0\1\0\2\0\3\0\4")
 (check-equal? (send (decode la2 (open-input-bytes #"\0\1\0\2\0\3\0\4")) to-list) '(1 2 3 4)))

