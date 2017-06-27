#lang restructure/racket
(require "array.rkt" "stream.rkt" "number.rkt" rackunit)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Array.coffee
|#

;describe 'Array', ->
;  describe 'decode', ->
;    it 'should decode fixed length', ->
;      stream = new DecodeStream new Buffer [1, 2, 3, 4, 5]
;      array = new ArrayT uint8, 4
;      array.decode(stream).should.deep.equal [1, 2, 3, 4]

(let ([stream (+DecodeStream (bytes 1 2 3 4 5))]
      [array (+Array uint8 4)])
  (check-equal? (send array decode stream) '(1 2 3 4)))

;; todo
;    it 'should decode fixed amount of bytes', ->
;      stream = new DecodeStream new Buffer [1, 2, 3, 4, 5]
;      array = new ArrayT uint16, 4, 'bytes'
;      array.decode(stream).should.deep.equal [258, 772]
(let ([stream (+DecodeStream (bytes 1 2 3 4 5))]
        [array (+Array uint16be 4 'bytes)])
    (check-equal? (send array decode stream) '(258 772)))

;; todo
;    it 'should decode length from parent key', ->
;      stream = new DecodeStream new Buffer [1, 2, 3, 4, 5]
;      array = new ArrayT uint8, 'len'
;      array.decode(stream, len: 4).should.deep.equal [1, 2, 3, 4]
;
#;(let ([stream (+DecodeStream (bytes 1 2 3 4 5))]
        [array (+Array uint8 4 'len)])
    (check-equal? (send array decode stream (mhash 'len 4)) '(1 2 3 4)))

;; todo
;    it 'should decode amount of bytes from parent key', ->
;      stream = new DecodeStream new Buffer [1, 2, 3, 4, 5]
;      array = new ArrayT uint16, 'len', 'bytes'
;      array.decode(stream, len: 4).should.deep.equal [258, 772]
#;(let ([stream (+DecodeStream (bytes 1 2 3 4 5))]
        [array (+Array uint16be 'len 'bytes)])
    (check-equal? (send array decode stream (mhash 'len 4)) '(258 772)))


;; todo
;    it 'should decode length as number before array', ->
;      stream = new DecodeStream new Buffer [4, 1, 2, 3, 4, 5]
;      array = new ArrayT uint8, uint8
;      array.decode(stream).should.deep.equal [1, 2, 3, 4]
#;(let ([stream (+DecodeStream (bytes 1 2 3 4 5))]
        [array (+Array uint8 uint8)])
    (check-equal? (send array decode stream (mhash 'len 4)) '(1 2 3 4)))


;; todo
;    it 'should decode amount of bytes as number before array', ->
;      stream = new DecodeStream new Buffer [4, 1, 2, 3, 4, 5]
;      array = new ArrayT uint16, uint8, 'bytes'
;      array.decode(stream).should.deep.equal [258, 772]
#;(let ([stream (+DecodeStream (bytes 4 1 2 3 4 5))]
        [array (+Array uint16be uint8 'bytes)])
    (check-equal? (send array decode stream) '(258 772)))

;    it 'should decode length from function', ->
;      stream = new DecodeStream new Buffer [1, 2, 3, 4, 5]
;      array = new ArrayT uint8, -> 4
;      array.decode(stream).should.deep.equal [1, 2, 3, 4]
#;(let ([stream (+DecodeStream (bytes 1 2 3 4 5))]
      [array (+Array uint8 (λ _ 4))])
  (check-equal? (send array decode stream) '(1 2 3 4)))

;; todo
;    it 'should decode amount of bytes from function', ->
;      stream = new DecodeStream new Buffer [1, 2, 3, 4, 5]
;      array = new ArrayT uint16, (-> 4), 'bytes'
;      array.decode(stream).should.deep.equal [258, 772]
#;(let ([stream (+DecodeStream (bytes 4 1 2 3 4 5))]
        [array (+Array uint16be (λ _ 4) 'bytes)])
    (check-equal? (send array decode stream) '(258 772)))

;; todo
;    it 'should decode to the end of the parent if no length is given', ->
;      stream = new DecodeStream new Buffer [1, 2, 3, 4, 5]
;      array = new ArrayT uint8
;      array.decode(stream, _length: 4, _startOffset: 0).should.deep.equal [1, 2, 3, 4]
#;(let ([stream (+DecodeStream (bytes 1 2 3 4 5))]
        [array (+Array uint8)])
    (check-equal? (send array decode stream (mhash '_length 4 '_startOffset 0)) '(1 2 3 4)))


;    it 'should decode to the end of the stream if no parent and length is given', ->
;      stream = new DecodeStream new Buffer [1, 2, 3, 4]
;      array = new ArrayT uint8
;      array.decode(stream).should.deep.equal [1, 2, 3, 4]
#;(let ([stream (+DecodeStream (bytes 1 2 3 4))]
      [array (+Array uint8)])
  (check-equal? (send array decode stream) '(1 2 3 4)))


;  describe 'size', ->
;    it 'should use array length', ->
;      array = new ArrayT uint8, 10
;      array.size([1, 2, 3, 4]).should.equal 4
(let ([array (+Array uint8 10)])
  (check-equal? (send array size '(1 2 3 4)) 4))

;; todo
;    it 'should add size of length field before string', ->
;      array = new ArrayT uint8, uint8
;      array.size([1, 2, 3, 4]).should.equal 5
;      
#;(let ([array (+Array uint8 uint8)])
  (check-equal? (send array size '(1 2 3 4)) 5))


;    it 'should use defined length if no value given', ->
;      array = new ArrayT uint8, 10
;      array.size().should.equal 10
(let ([array (+Array uint8 10)])
  (check-equal? (send array size) 10))




;  describe 'encode', ->
;    it 'should encode using array length', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [1, 2, 3, 4]
;        done()
;
;      array = new ArrayT uint8, 10
;      array.encode(stream, [1, 2, 3, 4])
;      stream.end()
;
;    it 'should encode length as number before array', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [4, 1, 2, 3, 4]
;        done()
;
;      array = new ArrayT uint8, uint8
;      array.encode(stream, [1, 2, 3, 4])
;      stream.end()
;
;    it 'should add pointers after array if length is encoded at start', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [4, 5, 6, 7, 8, 1, 2, 3, 4]
;        done()
;
;      array = new ArrayT new Pointer(uint8, uint8), uint8
;      array.encode(stream, [1, 2, 3, 4])
;      stream.end()