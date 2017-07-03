#lang restructure/test/racket

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Enum.coffee
|#

;describe 'Enum', ->
;  e = new Enum uint8, ['foo', 'bar', 'baz']
;  it 'should have the right size', ->
;    e.size().should.equal 1

(define e (+Enum uint8 '("foo" "bar" "baz")))

(check-equal? (send e size) 1)


;
;  it 'should decode', ->
;    stream = new DecodeStream new Buffer [1, 2, 0]
;    e.decode(stream).should.equal 'bar'
;    e.decode(stream).should.equal 'baz'
;    e.decode(stream).should.equal 'foo'

(let ([stream (+DecodeStream (+Buffer '(1 2 0)))])
  (check-equal? (send e decode stream) "bar")
  (check-equal? (send e decode stream) "baz")
  (check-equal? (send e decode stream) "foo"))

;
;  it 'should encode', (done) ->
;    stream = new EncodeStream
;    stream.pipe concat (buf) ->
;      buf.should.deep.equal new Buffer [1, 2, 0]
;      done()
;
;    e.encode stream, 'bar'
;    e.encode stream, 'baz'
;    e.encode stream, 'foo'
;    stream.end()

(let ([stream (+EncodeStream)])
  (send e encode stream "bar")
  (send e encode stream "baz")
  (send e encode stream "foo")
  (check-equal? (send stream dump) (+Buffer '(1 2 0))))

;
;  it 'should throw on unknown option', ->
;    stream = new EncodeStream
;    should.throw ->
;      e.encode stream, 'unknown'

(check-exn exn:fail:contract? (λ () (send e encode (+EncodeStream) "unknown")))