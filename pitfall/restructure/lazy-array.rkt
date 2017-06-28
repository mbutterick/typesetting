#lang restructure/racket
(require (prefix-in utils- "utils.rkt") "array.rkt" "number.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/LazyArray.coffee
|#

(define-subclass object% (InnerLazyArray type [length_ #f] [stream #f] [ctx #f])
  (define base (and stream (· stream pos)))
  (define items (mhash)) ;  rather than empty array

  (define/public-final (get index)
    (cond
      [(or (< index 0) (>= index length_)) #f]
      [else
       (define item (with-handlers ([exn:fail? (λ _ #f)])
                      (ref items index)))
       (or item
           (let ()
             (define pos_ (· stream pos))
             (send stream pos (+ base (* (send type size #f ctx) index)))
             (define new-item (send type decode stream ctx))
             (ref-set! items index new-item)
             (send stream pos pos_)
             new-item))]))

  (define/public-final (toArray)
    (for/list ([i (in-range length_)])
      (get i)))

  (define/public-final (inspect)
    (format "~a" (toArray))))

(define-subclass ArrayT (LazyArray)
  (inherit-field length_ type)
  
  (define/override (decode stream [parent #f])
    (define pos (· stream pos))
    (define length__ (utils-resolveLength length_ stream parent))

    (when (NumberT? length_)
      ;; define hidden properties
      (set! parent (mhash 'parent parent
                          '_startOffset pos
                          '_currentOffset 0
                          '_length length_)))
    
    (define res (+InnerLazyArray type length__ stream parent))
    (send stream pos (+ (· stream pos) (* length__ (send type size #f parent))))
    res)

  (define/override (size [val #f] [ctx #f])
    (super size (if (InnerLazyArray? val)
                    (send val toArray)
                    val) ctx))

  (define/override (encode stream val [ctx #f])
    (super encode stream (if (InnerLazyArray? val)
                             (send val toArray)
                             val) ctx)))    

#;(test-module
   (require "stream.rkt")
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

