#lang restructure/racket
(require "number.rkt" "utils.rkt" "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Array.coffee
|#

(define-subclass Streamcoder (Array type [length_ #f] [lengthType 'count])
          
  (define/augride (decode stream [parent #f])
    
    (define pos (send stream pos))

    (define res (make-object RestructureBase))
    (define ctx parent)

    (define length__
      (and length_ (resolveLength length_ stream parent)))

    (when (NumberT? length_)
      ;; define hidden properties
      (ref-set*! res 'parent parent
                 '_startOffset pos
                 '_currentOffset 0
                 '_length length_)
      (set! ctx res))


    )
  #|
    (cond
      [(or (not length__) (eq? lengthType 'bytes))
       (define target (cond
                        [length__ (+ (send stream pos) length__)]
                        [(and parent (· parent _length))
                         (+ (· parent _startOffset)
                            (· parent _length))]
                        [else
                         (*length stream)]))
       (while (< (send stream pos) target)
              (
          
               #;(define length__ (cond
                                    ;; explicit length
                                    [length_ (resolveLength length_ stream parent)]
                                    [else  ;; implicit length: length of stream divided by size of item
                                     (define num (send stream length))
                                     (define denom (send type size))
                                     (unless (andmap (λ (x) (and x (number? x))) (list num denom))
                                       (raise-argument-error 'Array:decode "valid length and size" (list num denom)))
                                     (floor (/ (send stream length) (send type size)))]))
    
    
    
               #;(define res (caseq lengthType
                                    [(bytes) (error 'array-decode-bytes-no!)]
                                    [(count) (for/list ([i (in-range length__)])
                                               (send type decode stream ctx))]))
               res)
|#

  (define/override (size [array #f])
    (when (and array (not (list? array)))
      (raise-argument-error 'Array:size "list" array))
    (cond
      [(not array) (* (send type size) (resolveLength length_ (+DecodeStream) #f))]
      [(Number? length_) (send length_ size)]
      [else (* (send type size) (length array))]))

  (define/augride (encode stream array [parent #f])
    (unless (list? array) (raise-argument-error 'Array:encode "list" array))
    (for ([item (in-list array)])
      (send type encode stream item))))

(define a (+Array uint8))
(define stream (+DecodeStream #"ABCDEFG"))
(send a decode stream)


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
    (define len (resolveLength length_ stream parent))
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


