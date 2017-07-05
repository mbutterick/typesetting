#lang reader (submod "racket.rkt" reader)


#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Buffer.coffee
|#

;describe 'Buffer', ->
;  describe 'decode', ->
;    it 'should decode', ->
(parameterize ([current-input-port (open-input-bytes (bytes #xab #xff #x1f #xb6))])
  (define buf (+BufferT 2))
  (check-equal? (decode buf) (bytes #xab #xff))
  (check-equal? (decode buf) (bytes #x1f #xb6)))


;    it 'should decode with parent key length', ->
(parameterize ([current-input-port (open-input-bytes (bytes #xab #xff #x1f #xb6))])
  (define buf (+BufferT 'len))
  (check-equal? (decode buf #:parent (hash 'len 3)) (bytes #xab #xff #x1f))
  (check-equal? (decode buf #:parent (hash 'len 1)) (bytes #xb6)))


;  describe 'size', ->
;    it 'should return size', ->
(check-equal? (size (+BufferT 2) (bytes #xab #xff)) 2)


;    it 'should use defined length if no value given', ->x
(check-equal? (size (+BufferT 10)) 10)


;  describe 'encode', ->
;    it 'should encode', (done) ->
(let ([buf (+BufferT 2)])
  (check-equal? (bytes-append
                 (encode buf (bytes #xab #xff) #f)
                 (encode buf (bytes #x1f #xb6) #f)) (bytes #xab #xff #x1f #xb6)))


;    it 'should encode length before buffer', (done) ->
(check-equal? (encode (+BufferT uint8) (bytes #xab #xff) #f) (bytes 2 #xab #xff))