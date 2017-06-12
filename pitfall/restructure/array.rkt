#lang restructure/racket
(require "number.rkt" "utils.rkt" "stream.rkt")
(provide (all-defined-out))

#|
approximates
https://github.com/mbutterick/restructure/blob/master/src/Array.coffee
|#

(define-subclass Streamcoder (Array type [_length #f] [lengthType 'count])
          
  (define/augment (decode stream [parent #f])
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

  (define/override (size array)
    (unless (list? array) (raise-argument-error 'Array:size "list" array))
    (* (send type size) (length array)))

  (define/augment (encode stream array [parent #f])
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