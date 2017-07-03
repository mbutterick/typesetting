#lang reader (submod "racket.rkt" reader)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Optional.coffee
|#

;describe 'Optional', ->
;  describe 'decode', ->
;    it 'should not decode when condition is falsy', ->
;      stream = new DecodeStream new Buffer [0]
;      optional = new Optional uint8, false
;      should.not.exist optional.decode(stream)
;      stream.pos.should.equal 0

(let ([stream (+DecodeStream (+Buffer '(0)))]
      [optional (+Optional uint8 #f)])
  (check-equal? (send optional decode stream) (void))
  (check-equal? (· stream pos) 0))


;    it 'should not decode when condition is a function and falsy', ->
;      stream = new DecodeStream new Buffer [0]
;      optional = new Optional uint8, -> false
;      should.not.exist optional.decode(stream)
;      stream.pos.should.equal 0

(let ([stream (+DecodeStream (+Buffer '(0)))]
      [optional (+Optional uint8 (λ _ #f))])
  (check-equal? (send optional decode stream) (void))
  (check-equal? (· stream pos) 0))

;
;    it 'should decode when condition is omitted', ->
;      stream = new DecodeStream new Buffer [0]
;      optional = new Optional uint8
;      should.exist optional.decode(stream)
;      stream.pos.should.equal 1

(let ([stream (+DecodeStream (+Buffer '(0)))]
      [optional (+Optional uint8)])
  (check-not-equal? (send optional decode stream) (void))
  (check-equal? (· stream pos) 1))

;
;    it 'should decode when condition is truthy', ->
;      stream = new DecodeStream new Buffer [0]
;      optional = new Optional uint8, true
;      should.exist optional.decode(stream)
;      stream.pos.should.equal 1

(let ([stream (+DecodeStream (+Buffer '(0)))]
      [optional (+Optional uint8 #t)])
  (check-not-equal? (send optional decode stream) (void))
  (check-equal? (· stream pos) 1))

;
;    it 'should decode when condition is a function and truthy', ->
;      stream = new DecodeStream new Buffer [0]
;      optional = new Optional uint8, -> true
;      should.exist optional.decode(stream)
;      stream.pos.should.equal 1


(let ([stream (+DecodeStream (+Buffer '(0)))]
      [optional (+Optional uint8 (λ _ #t))])
  (check-not-equal? (send optional decode stream) (void))
  (check-equal? (· stream pos) 1))

;
;  describe 'size', ->
;    it 'should return 0 when condition is falsy', ->
;      stream = new DecodeStream new Buffer [0]
;      optional = new Optional uint8, false
;      optional.size().should.equal 0

(let ([stream (+DecodeStream (+Buffer '(0)))]
      [optional (+Optional uint8 #f)])
  (check-equal? (· optional size) 0))

;
;    it 'should return 0 when condition is a function and falsy', ->
;      stream = new DecodeStream new Buffer [0]
;      optional = new Optional uint8, -> false
;      optional.size().should.equal 0

(let ([stream (+DecodeStream (+Buffer '(0)))]
      [optional (+Optional uint8 (λ _ #f))])
  (check-equal? (· optional size) 0))

;
;    it 'should return given type size when condition is omitted', ->
;      stream = new DecodeStream new Buffer [0]
;      optional = new Optional uint8
;      optional.size().should.equal 1

(let ([stream (+DecodeStream (+Buffer '(0)))]
      [optional (+Optional uint8)])
  (check-equal? (· optional size) 1))
;
;    it 'should return given type size when condition is truthy', ->
;      stream = new DecodeStream new Buffer [0]
;      optional = new Optional uint8, true
;      optional.size().should.equal 1

(let ([stream (+DecodeStream (+Buffer '(0)))]
      [optional (+Optional uint8 #t)])
  (check-equal? (· optional size) 1))

;
;    it 'should return given type size when condition is a function and truthy', ->
;      stream = new DecodeStream new Buffer [0]
;      optional = new Optional uint8, -> true
;      optional.size().should.equal 1

(let ([stream (+DecodeStream (+Buffer '(0)))]
      [optional (+Optional uint8 (λ _ #t))])
  (check-equal? (· optional size) 1))

;
;  describe 'encode', ->
;    it 'should not encode when condition is falsy', (done) ->
;      stream = new EncodeStream
;      optional = new Optional uint8, false
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal []
;        done()
;
;      optional.encode stream, 128
;      stream.end()

(let ([stream (+EncodeStream)]
      [optional (+Optional uint8 #f)])
  (send optional encode stream 128)
  (check-equal? (send stream dump) (+Buffer empty)))

;
;    it 'should not encode when condition is a function and falsy', (done) ->
;      stream = new EncodeStream
;      optional = new Optional uint8, -> false
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal []
;        done()
;
;      optional.encode stream, 128
;      stream.end()

(let ([stream (+EncodeStream)]
      [optional (+Optional uint8 (λ _ #f))])
  (send optional encode stream 128)
  (check-equal? (send stream dump) (+Buffer empty)))


;
;    it 'should encode when condition is omitted', (done) ->
;      stream = new EncodeStream
;      optional = new Optional uint8
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [128]
;        done()
;
;      optional.encode stream, 128
;      stream.end()

(let ([stream (+EncodeStream)]
      [optional (+Optional uint8)])
  (send optional encode stream 128)
  (check-equal? (send stream dump) (+Buffer '(128))))

;
;    it 'should encode when condition is truthy', (done) ->
;      stream = new EncodeStream
;      optional = new Optional uint8, true
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [128]
;        done()
;
;      optional.encode stream, 128
;      stream.end()

(let ([stream (+EncodeStream)]
      [optional (+Optional uint8 #t)])
  (send optional encode stream 128)
  (check-equal? (send stream dump) (+Buffer '(128))))

;
;    it 'should encode when condition is a function and truthy', (done) ->
;      stream = new EncodeStream
;      optional = new Optional uint8, -> true
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [128]
;        done()
;
;      optional.encode stream, 128
;      stream.end()

(let ([stream (+EncodeStream)]
      [optional (+Optional uint8 (λ _ #t))])
  (send optional encode stream 128)
  (check-equal? (send stream dump) (+Buffer '(128))))