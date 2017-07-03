#lang reader (submod "racket.rkt" reader)


#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/String.coffee
|#

;describe 'String', ->
;  describe 'decode', ->
;    it 'should decode fixed length', ->
;      stream = new DecodeStream new Buffer 'testing'
;      string = new StringT 7
;      string.decode(stream).should.equal 'testing'

(let ([stream (+DecodeStream (+Buffer "testing"))]
      [string (+StringT 7)])
  (check-equal? (decode string stream) "testing"))

;
;    it 'should decode length from parent key', ->
;      stream = new DecodeStream new Buffer 'testing'
;      string = new StringT 'len'
;      string.decode(stream, len: 7).should.equal 'testing'

(let ([stream (+DecodeStream (+Buffer "testing"))]
      [string (+StringT 'len)])
  (check-equal? (decode string stream (mhash 'len 7)) "testing"))


;
;    it 'should decode length as number before string', ->
;      stream = new DecodeStream new Buffer '\x07testing'
;      string = new StringT uint8
;      string.decode(stream).should.equal 'testing'

; octal \7 will print as \a
(let ([stream (+DecodeStream (+Buffer "\x07testing"))]
      [string (+StringT uint8)])
  (check-equal? (decode string stream (mhash 'len 7)) "testing"))

;
;    it 'should decode utf8', ->
;      stream = new DecodeStream new Buffer '🍻'
;      string = new StringT 4, 'utf8'
;      string.decode(stream).should.equal '🍻'

(let ([stream (+DecodeStream (+Buffer "🍻"))]
      [string (+StringT 4 'utf8)])
  (check-equal? (decode string stream) "🍻"))
;
;    it 'should decode encoding computed from function', ->
;      stream = new DecodeStream new Buffer '🍻'
;      string = new StringT 4, -> 'utf8'
;      string.decode(stream).should.equal '🍻'

(let ([stream (+DecodeStream (+Buffer "🍻"))]
      [string (+StringT 4 (λ _ 'utf8))])
  (check-equal? (decode string stream) "🍻"))

;
;    it 'should decode null-terminated string and read past terminator', ->
;      stream = new DecodeStream new Buffer '🍻\x00'
;      string = new StringT null, 'utf8'
;      string.decode(stream).should.equal '🍻'
;      stream.pos.should.equal 5

(let ([stream (+DecodeStream (+Buffer "🍻\x00"))]
      [string (+StringT #f 'utf8)])
  (check-equal? (decode string stream) "🍻")
  (check-equal? (pos stream) 5))

;
;    it 'should decode remainder of buffer when null-byte missing', ->
;      stream = new DecodeStream new Buffer '🍻'
;      string = new StringT null, 'utf8'
;      string.decode(stream).should.equal '🍻'

(let ([stream (+DecodeStream (+Buffer "🍻"))]
      [string (+StringT #f 'utf8)])
  (check-equal? (decode string stream) "🍻"))

;
;  describe 'size', ->
;    it 'should use string length', ->
;      string = new StringT 7
;      string.size('testing').should.equal 7

(let ([string (+StringT 7)])
  (check-equal? (size string "testing") 7))

;
;    it 'should use correct encoding', ->
;      string = new StringT 10, 'utf8'
;      string.size('🍻').should.equal 4

(let ([string (+StringT 10 'utf8)])
  (check-equal? (size string "🍻") 4))

;
;    it 'should use encoding from function', ->
;      string = new StringT 10, -> 'utf8'
;      string.size('🍻').should.equal 4

(let ([string (+StringT 10 (λ _ 'utf8))])
  (check-equal? (size string "🍻") 4))

;
;    it 'should add size of length field before string', ->
;      string = new StringT uint8, 'utf8'
;      string.size('🍻').should.equal 5

(let ([string (+StringT uint8 'utf8)])
  (check-equal? (size string "🍻") 5))

; todo
;    it 'should work with utf16be encoding', ->
;      string = new StringT 10, 'utf16be'
;      string.size('🍻').should.equal 4


;
;    it 'should take null-byte into account', ->
;      string = new StringT null, 'utf8'
;      string.size('🍻').should.equal 5

(let ([string (+StringT #f 'utf8)])
  (check-equal? (size string "🍻") 5))

;      
;    it 'should use defined length if no value given', ->
;      array = new StringT 10
;      array.size().should.equal 10

(let ([string (+StringT 10)])
  (check-equal? (size string) 10))

;      
;  describe 'encode', ->
;    it 'should encode using string length', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer 'testing'
;        done()
;
;      string = new StringT 7
;      string.encode(stream, 'testing')
;      stream.end()

(let ([string (+StringT 7)]
      [stream (+EncodeStream)])
  (encode string stream "testing")
  (check-equal? (dump stream) #"testing"))


;
;    it 'should encode length as number before string', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer '\x07testing'
;        done()
;
;      string = new StringT uint8
;      string.encode(stream, 'testing')
;      stream.end()

(let ([string (+StringT uint8)]
      [stream (+EncodeStream)])
  (encode string stream "testing")
  (check-equal? (dump stream) #"\x07testing"))

;
;    it 'should encode length as number before string utf8', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer '\x0ctesting 😜', 'utf8'
;        done()
;
;      string = new StringT uint8, 'utf8'
;      string.encode(stream, 'testing 😜')
;      stream.end()

(let ([string (+StringT uint8 'utf8)]
      [stream (+EncodeStream)])
  (encode string stream "testing 😜")
  (check-equal? (dump stream) (+Buffer "\x0ctesting 😜" 'utf8)))

;
;    it 'should encode utf8', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer '🍻'
;        done()
;
;      string = new StringT 4, 'utf8'
;      string.encode(stream, '🍻')
;      stream.end()

(let ([string (+StringT 4 'utf8)]
      [stream (+EncodeStream)])
  (encode string stream "🍻")
  (check-equal? (dump stream) (+Buffer "🍻")))

;
;    it 'should encode encoding computed from function', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer '🍻'
;        done()
;
;      string = new StringT 4, -> 'utf8'
;      string.encode(stream, '🍻')
;      stream.end()

(let ([string (+StringT 4 (λ _ 'utf8))]
      [stream (+EncodeStream)])
  (encode string stream "🍻")
  (check-equal? (dump stream) (+Buffer "🍻")))

;
;    it 'should encode null-terminated string', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer '🍻\x00'
;        done()
;
;      string = new StringT null, 'utf8'
;      string.encode(stream, '🍻')
;      stream.end()


(let ([string (+StringT #f 'utf8)]
      [stream (+EncodeStream)])
  (encode string stream "🍻")
  (check-equal? (dump stream) (+Buffer "🍻\x00")))