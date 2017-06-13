#lang restructure/racket
(require "number.rkt" "utils.rkt" "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Array.coffee
|#

(define-subclass Streamcoder (Array type [_length #f] [lengthType 'count])
          
  (define/augride (decode stream [parent #f])
    (let ([len (cond
                 ;; explicit length
                 [_length (resolveLength _length stream parent)]
                 [else  ;; implicit length: length of stream divided by size of item
                  (define num (send stream length))
                  (define denom (send type size))
                  (unless (andmap (λ (x) (and x (number? x))) (list num denom))
                    (raise-argument-error 'Array:decode "valid length and size" (list num denom)))
                  (floor (/ (send stream length) (send type size)))])])
    
      (caseq lengthType
             [(count) (for/list ([i (in-range len)])
                        (send type decode stream this))])))

  (define/override (size [array #f])
    (report* _length array)
    (when (and array (not (list? array)))
      (raise-argument-error 'Array:size "list" array))
    (cond
      [(not array) (* (send type size) (resolveLength _length (+DecodeStream) #f))]
      [(Number? _length) (send _length size)]
      [else (* (send type size) (length array))]))

  (define/augride (encode stream array [parent #f])
    (unless (list? array) (raise-argument-error 'Array:encode "list" array))
    (for ([item (in-list array)])
      (send type encode stream item))))


(test-module
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
  (field [base (· stream pos)]
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
  (inherit-field _length type)
  (define/override (decode stream [parent #f])
    (define len (resolveLength _length stream parent))
    (define res (+InnerLazyArray type len stream parent))
    (send stream pos (+ (· stream pos) (* len (send type size)))) ; skip the bytes that LazyArray would occupy
    res)

  (define/override (size [val #f])
    (super size (if (InnerLazyArray? val)
                    (send val toArray)
                    val)))

  (define/override (encode stream val)
    (super encode (if (InnerLazyArray? val)
                      (send val toArray)
                      val))))    

(test-module
 (define bstr #"ABCD1234")
 (define ds (+DecodeStream bstr))
 (define la (+LazyArray uint8 4))
 (define ila (send la decode ds))
 (check-equal? (send ds pos) 4)
 (check-equal? (send ila get 1) 66)
 (check-equal? (send ila get 3) 68)
 (check-equal? (send ds pos) 4)
 (check-equal? (send ila toArray) '(65 66 67 68)))
