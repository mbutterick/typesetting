#lang reader (submod "racket.rkt" reader)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/LazyArray.coffee
|#

;describe 'LazyArray', ->
;  describe 'decode', ->
;    it 'should decode items lazily', ->
;      stream = new DecodeStream new Buffer [1, 2, 3, 4, 5]
;      array = new LazyArray uint8, 4
;      
;      arr = array.decode(stream)
;      arr.should.not.be.an.instanceof Array
;      arr.should.have.length 4
;      stream.pos.should.equal 4
;      
;      arr.get(0).should.equal 1
;      arr.get(1).should.equal 2
;      arr.get(2).should.equal 3
;      arr.get(3).should.equal 4
;      
;      should.not.exist arr.get(-1)
;      should.not.exist arr.get(5)

(let* ([stream (+DecodeStream (+Buffer '(1 2 3 4 5)))]
       [array (+LazyArray uint8 4)])
  (define arr (decode array stream))
  (check-false (Array? arr))
  (check-equal? (ref arr 'len) 4)
  (check-equal? (pos stream) 4)
  (check-equal? (get arr 0) 1)
  (check-equal? (get arr 1) 2)
  (check-equal? (get arr 2) 3)
  (check-equal? (get arr 3) 4))



;      
;    it 'should be able to convert to an array', ->
;      stream = new DecodeStream new Buffer [1, 2, 3, 4, 5]
;      array = new LazyArray uint8, 4
;      
;      arr = array.decode(stream)
;      arr.toArray().should.deep.equal [1, 2, 3, 4]

(let* ([stream (+DecodeStream (+Buffer '(1 2 3 4 5)))]
       [array (+LazyArray uint8 4)])
  (define arr (decode array stream))
  (check-equal? (send arr to-list) '(1 2 3 4)))

;      
;    it 'should have an inspect method', ->
;      stream = new DecodeStream new Buffer [1, 2, 3, 4, 5]
;      array = new LazyArray uint8, 4
;      
;      arr = array.decode(stream)
;      arr.inspect().should.equal '[ 1, 2, 3, 4 ]'

#;(let* ([stream (+DecodeStream (+Buffer '(1 2 3 4 5)))]
       [array (+LazyArray uint8 4)])
  (define arr (decode array stream))
  (check-equal? (send arr inspect) (format "~a" '(1 2 3 4))))

;      
;    it 'should decode length as number before array', ->
;      stream = new DecodeStream new Buffer [4, 1, 2, 3, 4, 5]
;      array = new LazyArray uint8, uint8
;      arr = array.decode(stream)
;      
;      arr.toArray().should.deep.equal [1, 2, 3, 4]

(let* ([stream (+DecodeStream (+Buffer '(4 1 2 3 4 5)))]
       [array (+LazyArray uint8 uint8)])
  (define arr (decode array stream))
  (check-equal? (send arr to-list) '(1 2 3 4)))

;      
;  describe 'size', ->
;    it 'should work with LazyArrays', ->
;      stream = new DecodeStream new Buffer [1, 2, 3, 4, 5]
;      array = new LazyArray uint8, 4
;      arr = array.decode(stream)
;      
;      array.size(arr).should.equal 4

(let* ([stream (+DecodeStream (+Buffer '(1 2 3 4 5)))]
       [array (+LazyArray uint8 4)])
  (define arr (decode array stream))
  (check-equal? (size array arr) 4))

;      
;  describe 'encode', ->
;    it 'should work with LazyArrays', (done) ->
;      stream = new DecodeStream new Buffer [1, 2, 3, 4, 5]
;      array = new LazyArray uint8, 4
;      arr = array.decode(stream)
;      
;      enc = new EncodeStream
;      enc.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer [1, 2, 3, 4]
;        done()
;      
;      array.encode(enc, arr)
;      enc.end()

(let* ([stream (+DecodeStream (+Buffer '(1 2 3 4 5)))]
       [array (+LazyArray uint8 4)])
  (define arr (decode array stream))  
  (check-equal? (encode array #f arr) (+Buffer '(1 2 3 4))))