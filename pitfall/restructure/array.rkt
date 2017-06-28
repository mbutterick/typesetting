#lang restructure/racket
(require "number.rkt" (prefix-in utils- "utils.rkt") "stream.rkt" br/cond)
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Array.coffee
|#

(define-subclass Streamcoder (Array type [length_ #f] [lengthType 'count])
          
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
    (when array (unless (countable? array)
                  (raise-argument-error 'Array:size "list or countable" array)))
    (cond
      [(not array) (* (send type size #f ctx) (utils-resolveLength length_ #f ctx))]
      [else
       (define size 0)
       (when (NumberT? length_)
         (increment! size (send length_ size))
         (set! ctx (mhash 'parent ctx)))
       (for ([item (in-list (countable->list array))])
         (increment! size (send type size item ctx)))
       size]))

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
      (define i 0)
      (define ptr #f)
      (while (< i (length (· ctx pointers)))
             (set! ptr (list-ref (· ctx pointers) (increment! i)))
             (send (· ptr type) encode stream (· ptr val))))))


#;(test-module
   (define stream (+DecodeStream #"ABCDEFG"))
   
   (define A (+Array uint16be 3))
   (check-equal? (send A decode stream) '(16706 17220 17734))
   (define os (+EncodeStream))
   (send A encode os '(16706 17220 17734))
   (check-equal? (send os dump) #"ABCDEF")

   (check-equal? (send (+Array uint16be) size '(1 2 3)) 6)
   (check-equal? (send (+Array doublebe) size '(1 2 3 4 5)) 40))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/LazyArray.coffee
|#

(define-subclass object% (InnerLazyArray type [_length #f] [stream #f] [parent #f])
  (field [base (and stream (· stream pos))]
         [items (mhash)]) ; implement with hash (random add) rather than array

  (define/public-final (get index)
    (when (or (< index 0) (<= _length index))
      (raise-argument-error 'InnerLazyArray:get (format "array index between 0 and ~a" _length) index))
    (hash-ref! items index (λ ()
                             (define stashed-pos (· stream pos))
                             (send stream pos (+ base (* index (send type size))))
                             (define new-val (send type decode stream parent))
                             (send stream pos stashed-pos)
                             new-val)))

  (define/public-final (toArray)
    (for/list ([i (in-range _length)])
      (get i))))

(define-subclass Array (LazyArray)
  (inherit-field length_ type)
  (define/override (decode stream [parent #f])
    (define len (utils-resolveLength length_ stream parent))
    (define res (+InnerLazyArray type len stream parent))
    (define lazy-space (* len (send type size)))
    (report lazy-space)
    (send stream pos (+ (· stream pos) lazy-space)) ; skip the bytes that LazyArray would occupy
    res)

  (define/override (size [val #f])
    (super size (if (InnerLazyArray? val)
                    (send val toArray)
                    val)))

  (define/override (encode stream val)
    (super encode stream (if (InnerLazyArray? val)
                             (send val toArray)
                             val))))    

#;(test-module
   (define bstr #"ABCD1234")
   (define ds (+DecodeStream bstr))
   (define la (+LazyArray uint8 4))
   (define ila (send la decode ds))
   (check-equal? (send ds pos) 4)
   (check-equal? (send ila get 1) 66)
   (check-equal? (send ila get 3) 68)
   (check-equal? (send ds pos) 4)
   (check-equal? (send ila toArray) '(65 66 67 68))

   (define la2 (+LazyArray int16be (λ (t) 4)))
   (define es (+EncodeStream))
   (send la2 encode es '(1 2 3 4))
   (check-equal? (send es dump) #"\0\1\0\2\0\3\0\4")
   (check-equal? (send (send la2 decode (+DecodeStream #"\0\1\0\2\0\3\0\4")) toArray) '(1 2 3 4))

   )


