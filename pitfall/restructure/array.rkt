#lang restructure/racket
(require "number.rkt" (prefix-in utils- "utils.rkt") "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Array.coffee
|#

(define-subclass Streamcoder (ArrayT type [length_ #f] [lengthType 'count])
          
  (define/augride (decode stream [parent #f])
    (define pos (send stream pos))

    (define res (make-object RestructureBase)) ; instead of empty list
    (define ctx parent)

    (define length__
      (and length_ (utils-resolveLength length_ stream parent)))

    (when (NumberT? length_)
      ;; define hidden properties
      (ref-set*! res 'parent parent
                 '_startOffset pos
                 '_currentOffset 0
                 '_length length_)
      (set! ctx res))

    (cond
      [(or (not length__) (eq? lengthType 'bytes))
       (define target (cond
                        [length__ (+ (send stream pos) length__)]
                        [(and parent (· parent _length))
                         (+ (ref parent '_startOffset)
                            (ref parent '_length))]
                        [else (· stream length_)]))
       (ref-set! res '_list
                 (push-end res
                           (for/list ([i (in-naturals)]
                                      #:break (= (send stream pos) target))
                             (send type decode stream ctx))))]
      [else
       (ref-set! res '_list
                 (push-end res
                           (for/list ([i (in-range length__)])
                             (send type decode stream ctx))))])

    (countable->list res))

  (define/override (size [array #f] [ctx #f])
    (when array
      (unless (countable? array)
        (raise-argument-error 'Array:size "list or countable" array)))
    (cond
      [(not array)
       (* (send type size #f ctx) (utils-resolveLength length_ #f ctx))]
      [else
       (+ (cond
            [(NumberT? length_)
             (set! ctx (mhash 'parent ctx))
             (send length_ size)]
            [else 0])
          (for/sum ([item (in-list (countable->list array))])
            (send type size item ctx)))]))

  (define/augride (encode stream array [parent #f])
    (when array (unless (countable? array)
                  (raise-argument-error 'Array:encode "list or countable" array)))
    (define ctx parent)
    (when (NumberT? length_)
      (set! ctx (mhash 'pointers null
                       'startOffset (· stream pos)
                       'parent parent))
      (ref-set! ctx 'pointerOffset (+ (· stream pos) (size array ctx)))
      (send length_ encode stream (length array)))

    (for ([item (in-list (countable->list array))])
      (send type encode stream item ctx))

    (when (NumberT? length_)
      (for ([ptr (in-list (· ctx pointers))])
        (send (· ptr type) encode stream (· ptr val))))))

(define-values (Array? +Array) (values ArrayT? +ArrayT))

(test-module
 (define stream (+DecodeStream #"ABCDEFG"))
 (define A (+Array uint16be 3))
 (check-equal? (send A decode stream) '(16706 17220 17734))
 (check-equal? (send A encode #f '(16706 17220 17734)) #"ABCDEF")
 (check-equal? (send (+Array uint16be) size '(1 2 3)) 6)
 (check-equal? (send (+Array doublebe) size '(1 2 3 4 5)) 40))
