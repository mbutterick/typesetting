#lang restructure/racket
(require "array.rkt" "stream.rkt" "number.rkt" rackunit)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Array.coffee
|#

;; it 'should decode fixed length', ->
(let ([stream (+DecodeStream (bytes 1 2 3 4 5))]
      [array (+Array uint8 4)])
  (check-equal? (send array decode stream) '(1 2 3 4)))

;; todo
;; it 'should decode fixed amount of bytes', ->
#;(let ([stream (+DecodeStream (bytes 1 2 3 4 5))]
        [array (+Array uint16be 4 'bytes)])
    (check-equal? (send array decode stream) '(258 772)))

;; todo
;; it 'should decode length from parent key', ->
#;(let ([stream (+DecodeStream (bytes 1 2 3 4 5))]
        [array (+Array uint8 4 'len)])
    (check-equal? (send array decode stream (mhash 'len 4)) '(1 2 3 4)))

;; todo
;; it 'should decode amount of bytes from parent key', ->
#;(let ([stream (+DecodeStream (bytes 1 2 3 4 5))]
        [array (+Array uint16be 'len 'bytes)])
    (check-equal? (send array decode stream (mhash 'len 4)) '(258 772)))


;; todo
;; it 'should decode length as number before array', ->
#;(let ([stream (+DecodeStream (bytes 1 2 3 4 5))]
        [array (+Array uint8 uint8)])
    (check-equal? (send array decode stream (mhash 'len 4)) '(1 2 3 4)))


;; todo
;; it 'should decode amount of bytes as number before array', ->
#;(let ([stream (+DecodeStream (bytes 4 1 2 3 4 5))]
        [array (+Array uint16be uint8 'bytes)])
    (check-equal? (send array decode stream) '(258 772)))

;; it 'should decode length from function', ->
(let ([stream (+DecodeStream (bytes 1 2 3 4 5))]
      [array (+Array uint8 (λ _ 4))])
  (check-equal? (send array decode stream) '(1 2 3 4)))

;; todo
;; it 'should decode amount of bytes from function', ->
#;(let ([stream (+DecodeStream (bytes 4 1 2 3 4 5))]
        [array (+Array uint16be (λ _ 4) 'bytes)])
    (check-equal? (send array decode stream) '(258 772)))

;; todo
;; it 'should decode to the end of the parent if no length is given', ->
#;(let ([stream (+DecodeStream (bytes 1 2 3 4 5))]
        [array (+Array uint8)])
    (check-equal? (send array decode stream (mhash '_length 4 '_startOffset 0)) '(1 2 3 4)))


;; it 'should decode to the end of the stream if no parent and length is given', ->
(let ([stream (+DecodeStream (bytes 1 2 3 4))]
      [array (+Array uint8)])
  (check-equal? (send array decode stream) '(1 2 3 4)))


;; it 'should use array length', ->
(let ([array (+Array uint8 10)])
  (check-equal? (send array size '(1 2 3 4)) 4))

;; todo
;; it 'should add size of length field before string', ->
#;(let ([array (+Array uint8 uint8)])
  (check-equal? (send array size '(1 2 3 4)) 5))


;; it 'should use defined length if no value given', ->
(let ([array (+Array uint8 10)])
  (check-equal? (send array size) 10))