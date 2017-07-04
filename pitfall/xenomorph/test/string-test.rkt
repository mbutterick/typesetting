#lang reader (submod "racket.rkt" reader)


#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/String.coffee
|#

;describe 'String', ->
;  describe 'decode', ->
;    it 'should decode fixed length', ->

(parameterize ([current-input-port (open-input-bytes #"testing")])
  (check-equal? (decode (+StringT 7)) "testing"))


;    it 'should decode length from parent key', ->

(parameterize ([current-input-port (open-input-bytes #"testing")])
  (check-equal? (decode (+StringT 'len) #:parent (mhash 'len 7)) "testing"))


;    it 'should decode length as number before string', ->

(parameterize ([current-input-port (open-input-bytes #"\x07testing")])
  (check-equal? (decode (+StringT uint8) #:parent (mhash 'len 7)) "testing"))


;;    it 'should decode utf8', ->

(parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "🍻"))])
  (check-equal? (decode (+StringT 4 'utf8)) "🍻"))

;;    it 'should decode encoding computed from function', ->

(parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "🍻"))])
  (check-equal? (decode (+StringT 4 (λ _ 'utf8))) "🍻"))


;    it 'should decode null-terminated string and read past terminator', ->

(parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "🍻\x00"))])
  (check-equal? (decode (+StringT #f 'utf8)) "🍻")
  (check-equal? (pos (current-input-port)) 5))


;    it 'should decode remainder of buffer when null-byte missing', ->

(parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "🍻"))])
  (check-equal? (decode (+StringT #f 'utf8)) "🍻"))


;  describe 'size', ->
;    it 'should use string length', ->

(check-equal? (size (+StringT 7) "testing") 7)


;    it 'should use correct encoding', ->

(check-equal? (size (+StringT 10 'utf8) "🍻") 4)


;    it 'should use encoding from function', ->

(check-equal? (size (+StringT 10 (λ _ 'utf8)) "🍻") 4)


;    it 'should add size of length field before string', ->

(check-equal? (size (+StringT uint8 'utf8) "🍻") 5)


; todo
;    it 'should work with utf16be encoding', ->


;    it 'should take null-byte into account', ->

(check-equal? (size (+StringT #f 'utf8) "🍻") 5)

      
;    it 'should use defined length if no value given', ->

(check-equal? (size (+StringT 10)) 10)

;      
;  describe 'encode', ->
;    it 'should encode using string length', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (encode (+StringT 7) "testing")
  (check-equal? (dump (current-output-port)) #"testing"))


;    it 'should encode length as number before string', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (encode (+StringT uint8) "testing")
  (check-equal? (dump (current-output-port)) #"\x07testing"))


;    it 'should encode length as number before string utf8', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (encode (+StringT uint8 'utf8) "testing 😜")
  (check-equal? (dump (current-output-port)) (string->bytes/utf-8 "\x0ctesting 😜")))


;    it 'should encode utf8', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (encode (+StringT 4 'utf8) "🍻" )
  (check-equal? (dump (current-output-port)) (string->bytes/utf-8 "🍻")))


;    it 'should encode encoding computed from function', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (encode (+StringT 4 (λ _ 'utf8)) "🍻")
  (check-equal? (dump (current-output-port)) (string->bytes/utf-8 "🍻")))


;    it 'should encode null-terminated string', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (encode (+StringT #f 'utf8) "🍻"  )
  (check-equal? (dump (current-output-port)) (string->bytes/utf-8 "🍻\x00")))