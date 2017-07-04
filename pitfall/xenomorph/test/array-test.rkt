#lang reader (submod "racket.rkt" reader)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Array.coffee
|#

;describe 'Array', ->
;  describe 'decode', ->
;    it 'should decode fixed length', ->
(parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
  (check-equal? (decode (+ArrayT uint8 4)) '(1 2 3 4)))


;    it 'should decode fixed amount of bytes', ->
(parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
  (check-equal? (decode (+ArrayT uint16be 4 'bytes)) '(258 772)))


;    it 'should decode length from parent key', ->
(parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
  (check-equal? (decode (+ArrayT uint8 'len) #:parent (mhash 'len 4)) '(1 2 3 4)))


;    it 'should decode amount of bytes from parent key', ->
(parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
  (check-equal? (decode (+ArrayT uint16be 'len 'bytes) #:parent (mhash 'len 4)) '(258 772)))


;    it 'should decode length as number before array', ->
(parameterize ([current-input-port (open-input-bytes (bytes 4 1 2 3 4 5))])
  (check-equal? (decode (+ArrayT uint8 uint8)) '(1 2 3 4)))


;    it 'should decode amount of bytes as number before array', ->
(parameterize ([current-input-port (open-input-bytes (bytes 4 1 2 3 4 5))])
  (check-equal? (decode (+ArrayT uint16be uint8 'bytes)) '(258 772)))


;    it 'should decode length from function', ->
(parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
  (check-equal? (decode (+ArrayT uint8 (λ _ 4))) '(1 2 3 4)))


;    it 'should decode amount of bytes from function', ->
(parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
  (check-equal? (decode (+ArrayT uint16be (λ _ 4) 'bytes)) '(258 772)))


;    it 'should decode to the end of the parent if no length is given', ->
(parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
  (check-equal? (decode (+ArrayT uint8) #:parent (mhash '_length 4 '_startOffset 0)) '(1 2 3 4)))


; decode to the end of the stream if parent exists, but its length is 0
(parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
  (check-equal? (decode (+ArrayT uint8) #:parent (mhash '_length 0 '_startOffset 0)) '(1 2 3 4 5)))


;    it 'should decode to the end of the stream if no parent and length is given', ->
(parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4))])
  (check-equal? (decode (+ArrayT uint8)) '(1 2 3 4 )))


;  describe 'size', ->
;    it 'should use array length', ->
(check-equal? (size (+ArrayT uint8 10) '(1 2 3 4)) 4)


;    it 'should add size of length field before string', ->
(check-equal? (size (+ArrayT uint8 uint8) '(1 2 3 4)) 5)


;    it 'should use defined length if no value given', ->
(check-equal? (size (+ArrayT uint8 10)) 10)


;  describe 'encode', ->
;    it 'should encode using array length', (done) ->
(check-equal? (encode (+ArrayT uint8 10) '(1 2 3 4) #f) (bytes 1 2 3 4))


;    it 'should encode length as number before array', (done) ->
(check-equal? (encode (+ArrayT uint8 uint8) '(1 2 3 4) #f) (bytes 4 1 2 3 4))


;    it 'should add pointers after array if length is encoded at start', (done) ->
(check-equal? (encode (+ArrayT (+Pointer uint8 uint8) uint8) '(1 2 3 4) #f) (bytes 4 5 6 7 8 1 2 3 4))