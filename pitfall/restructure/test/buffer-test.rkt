#lang reader (submod "racket.rkt" reader)


#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Buffer.coffee
|#

;describe 'Buffer', ->
;  describe 'decode', ->
;    it 'should decode', ->
;      stream = new DecodeStream new Buffer [0xab, 0xff, 0x1f, 0xb6]
;      buf = new BufferT(2)
;      buf.decode(stream).should.deep.equal new Buffer [0xab, 0xff]
;      buf.decode(stream).should.deep.equal new Buffer [0x1f, 0xb6]

(let ([stream (+DecodeStream (+Buffer '(#xab #xff #x1f #xb6)))]
      [buf (+BufferT 2)])
  (check-equal? (send buf decode stream) (+Buffer '(#xab #xff)))
  (check-equal? (send buf decode stream) (+Buffer '(#x1f #xb6))))


;
;    it 'should decode with parent key length', ->
;      stream = new DecodeStream new Buffer [0xab, 0xff, 0x1f, 0xb6]
;      buf = new BufferT('len')
;      buf.decode(stream, len: 3).should.deep.equal new Buffer [0xab, 0xff, 0x1f]
;      buf.decode(stream, len: 1).should.deep.equal new Buffer [0xb6]

(let ([stream (+DecodeStream (+Buffer '(#xab #xff #x1f #xb6)))]
      [buf (+BufferT 'len)])
  (check-equal? (send buf decode stream (hash 'len 3)) (+Buffer '(#xab #xff #x1f)))
  (check-equal? (send buf decode stream (hash 'len 1)) (+Buffer '(#xb6))))


;      
;  describe 'size', ->
;    it 'should return size', ->
;      buf = new BufferT(2)
;      buf.size(new Buffer [0xab, 0xff]).should.equal 2

(let ([buf (+BufferT 2)])
  (check-equal? (send buf size (+Buffer '(#xab #xff))) 2))

;
;    it 'should use defined length if no value given', ->
;      array = new BufferT 10
;      array.size().should.equal 10

(let ([array (+BufferT 10)])
  (check-equal? (send array size) 10))


;
;  describe 'encode', ->
;    it 'should encode', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [0xab, 0xff, 0x1f, 0xb6]
;        done()
;
;      buf = new BufferT(2)
;      buf.encode stream, new Buffer [0xab, 0xff]
;      buf.encode stream, new Buffer [0x1f, 0xb6]
;      stream.end()
;

(let ([stream (+EncodeStream)]
      [buf (+BufferT 2)])
  (send buf encode stream (+Buffer '(#xab #xff)))
  (send buf encode stream (+Buffer '(#x1f #xb6)))
  (check-equal? (send stream dump) (+Buffer '(#xab #xff #x1f #xb6))))


;    it 'should encode length before buffer', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [2, 0xab, 0xff]
;        done()
;
;      buf = new BufferT(uint8)
;      buf.encode stream, new Buffer [0xab, 0xff]
;      stream.end()

(let ([stream (+EncodeStream)]
      [buf (+BufferT uint8)])
  (send buf encode stream (+Buffer '(#xab #xff)))
  (check-equal? (send stream dump) (+Buffer '(2 #xab #xff))))