#lang reader (submod "racket.rkt" reader)
(require "number.rkt" "utils.rkt" "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Array.coffee
|#

(define-subclass Streamcoder (ArrayT type [len #f] [length-type 'count])
          
  (define/augride (decode stream [parent #f])
    (define ctx (if (NumberT? len)
                    (mhasheq 'parent parent
                             '_startOffset (· stream pos)
                             '_currentOffset 0
                             '_length len)
                    parent))

    (define decoded-len (resolve-length len stream parent))
    (cond
      [(or (not decoded-len) (eq? length-type 'bytes))
       (define end-pos (cond
                         ;; decoded-len is byte length
                         [decoded-len (+ (· stream pos) decoded-len)]
                         ;; no decoded-len, but parent has length
                         [(and parent (not (zero? (· parent _length)))) (+ (· parent _startOffset) (· parent _length))]
                         ;; no decoded-len or parent, so consume whole stream
                         [else (· stream length_)]))
       (for/list ([i (in-naturals)]
                  #:break (= (· stream pos) end-pos))
         (send type decode stream ctx))]
      ;; we have decoded-len, which is treated as count of items
      [else (for/list ([i (in-range decoded-len)])
              (send type decode stream ctx))]))
  

  (define/override (size [val #f] [ctx #f])
    (when val (unless (countable? val)
                (raise-argument-error 'Array:size "list or countable" val)))
    (cond
      [val (let-values ([(ctx len-size) (if (NumberT? len)
                                            (values (mhasheq 'parent ctx) (send len size))
                                            (values ctx 0))])
             (+ len-size (for/sum ([item (in-list (countable->list val))])
                           (send type size item ctx))))]
      [else (let ([item-count (resolve-length len #f ctx)]
                  [item-size (send type size #f ctx)])
              (* item-size item-count))]))
  

  (define/augride (encode stream array [parent #f])
    (when array (unless (countable? array)
                  (raise-argument-error 'Array:encode "list or countable" array)))

    (define (encode-items ctx)
      (for ([item (in-list (countable->list array))])
        (send type encode stream item ctx)))

    (cond
      [(NumberT? len) (define ctx (mhash 'pointers null
                                         'startOffset (· stream pos)
                                         'parent parent))
                      (ref-set! ctx 'pointerOffset (+ (· stream pos) (size array ctx)))
                      (send len encode stream (length array)) ; encode length at front
                      (encode-items ctx)
                      (for ([ptr (in-list (· ctx pointers))]) ; encode pointer data at end
                        (send (· ptr type) encode stream (· ptr val)))]
      [else (encode-items parent)])))

(define-values (Array Array? +Array) (values ArrayT ArrayT? +ArrayT))

(test-module
 (define stream (+DecodeStream #"ABCDEFG"))
 (define A (+Array uint16be 3))
 (check-equal? (send A decode stream) '(16706 17220 17734))
 (check-equal? (send A encode #f '(16706 17220 17734)) #"ABCDEF")
 (check-equal? (send (+Array uint16be) size '(1 2 3)) 6)
 (check-equal? (send (+Array doublebe) size '(1 2 3 4 5)) 40))
