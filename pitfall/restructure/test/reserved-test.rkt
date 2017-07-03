#lang reader (submod "racket.rkt" reader)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Reserved.coffee
|#

;describe 'Reserved', ->
;  it 'should have a default count of 1', ->
;    reserved = new Reserved uint8
;    reserved.size().should.equal 1

(let ([reserved (+Reserved uint8)])
  (check-equal? (size reserved) 1))

;
;  it 'should allow custom counts and types', ->
;    reserved = new Reserved uint16, 10
;    reserved.size().should.equal 20

(let ([reserved (+Reserved uint16be 10)])
  (check-equal? (size reserved) 20))

;
;  it 'should decode', ->
;    stream = new DecodeStream new Buffer [0, 0]
;    reserved = new Reserved uint16
;    should.not.exist reserved.decode(stream)
;    stream.pos.should.equal 2

(let ([stream (+DecodeStream (+Buffer '(0 0)))]
      [reserved (+Reserved uint16be)])
  (check-equal? (decode reserved stream) (void))
  (check-equal? (pos stream) 2))

;
;  it 'should encode', (done) ->
;    stream = new EncodeStream
;    reserved = new Reserved uint16
;    stream.pipe concat (buf) ->
;      buf.should.deep.equal new Buffer [0, 0]
;      done()
;
;    reserved.encode stream
;    stream.end()

(let ([stream (+EncodeStream)]
      [reserved (+Reserved uint16be)])
  (encode reserved stream)
  (check-equal? (dump stream) (+Buffer '(0 0))))