#lang restructure/racket
(require "struct.rkt" "string.rkt" "number.rkt" "buffer.rkt" "stream.rkt" rackunit)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Struct.coffee
|#


;describe 'Struct', ->
;  describe 'decode', ->
;    it 'should decode into an object', ->
;      stream = new DecodeStream new Buffer '\x05devon\x15'
;      struct = new Struct
;        name: new StringT uint8
;        age: uint8
;
;      struct.decode(stream).should.deep.equal
;        name: 'devon'
;        age: 21

(let ([stream (+DecodeStream (+Buffer "\x05devon\x15"))]
      [struct (+Struct (dictify 'name (+StringT uint8)
                                'age uint8))])
  (check-equal? (send (send struct decode stream) ht)
                (mhasheq 'name "devon" 'age 21)))


;
;    it 'should support process hook', ->
;      stream = new DecodeStream new Buffer '\x05devon\x20'
;      struct = new Struct
;        name: new StringT uint8
;        age: uint8
;
;      struct.process = ->
;        @canDrink = @age >= 21
;
;      struct.decode(stream).should.deep.equal
;        name: 'devon'
;        age: 32
;        canDrink: true

(let ([stream (+DecodeStream (+Buffer "\x05devon\x20"))]
      [struct (+Struct (dictify 'name (+StringT uint8)
                                'age uint8))])
  (set-field! process struct (λ (o stream) (ref-set! o 'canDrink (>= (ref o 'age) 21))))
  (check-equal? (send (send struct decode stream) ht)
                (mhasheq 'name "devon" 'age 32 'canDrink #t)))


;
;    it 'should support function keys', ->
;      stream = new DecodeStream new Buffer '\x05devon\x20'
;      struct = new Struct
;        name: new StringT uint8
;        age: uint8
;        canDrink: -> @age >= 21
;
;      struct.decode(stream).should.deep.equal
;        name: 'devon'
;        age: 32
;        canDrink: true

(let ([stream (+DecodeStream (+Buffer "\x05devon\x20"))]
      [struct (+Struct (dictify 'name (+StringT uint8)
                                'age uint8
                                'canDrink (λ (o) (>= (ref o 'age) 21))))])
  (check-equal? (send (send struct decode stream) ht)
                (mhasheq 'name "devon" 'age 32 'canDrink #t)))


;
;  describe 'size', ->
;    it 'should compute the correct size', ->
;      struct = new Struct
;        name: new StringT uint8
;        age: uint8
;
;      struct.size(name: 'devon', age: 21).should.equal 7

(let ([struct (+Struct (dictify 'name (+StringT uint8)
                                'age uint8))])
  (check-equal? (send struct size (hasheq 'name "devon" 'age 32)) 7))


; todo: when pointers are ready
;    it 'should compute the correct size with pointers', ->
;      struct = new Struct
;        name: new StringT uint8
;        age: uint8
;        ptr: new Pointer uint8, new StringT uint8
;
;      size = struct.size
;        name: 'devon'
;        age: 21
;        ptr: 'hello'
;
;      size.should.equal 14


(displayln 'warning:pointer-not-done)


;      
;    it 'should get the correct size when no value is given', ->
;      struct = new Struct
;        name: new StringT 4
;        age: uint8
;
;      struct.size().should.equal 5


(let ([struct (+Struct (dictify 'name (+StringT 4)
                                'age uint8))])
  (check-equal? (send struct size) 5))


;      
;    it 'should throw when getting non-fixed length size and no value is given', ->
;      struct = new Struct
;        name: new StringT uint8
;        age: uint8
;
;      should.throw ->
;        struct.size()
;      , /not a fixed size/i

(let ([struct (+Struct (dictify 'name (+StringT uint8)
                                'age uint8))])
  (check-exn exn:fail:contract? (λ () (send struct size))))

;      
;  describe 'encode', ->
;    it 'should encode objects to buffers', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer '\x05devon\x15'
;        done()
;
;      struct = new Struct
;        name: new StringT uint8
;        age: uint8
;
;      struct.encode stream,
;        name: 'devon'
;        age: 21
;
;      stream.end()

(let ([stream (+DecodeStream (+Buffer "\x05devon\x15"))]
      [struct (+Struct (dictify 'name (+StringT uint8)
                                'age uint8))])
  (check-equal? (send (send struct decode stream) ht)
                (mhasheq 'name "devon" 'age 21)))

;
;    it 'should support preEncode hook', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer '\x05devon\x15'
;        done()
;
;      struct = new Struct
;        nameLength: uint8
;        name: new StringT 'nameLength'
;        age: uint8
;
;      struct.preEncode = ->
;        @nameLength = @name.length
;
;      struct.encode stream,
;        name: 'devon'
;        age: 21
;
;      stream.end()

(let ([stream (+EncodeStream)]
      [struct (+Struct (dictify 'nameLength uint8
                                'name (+StringT 'nameLength)
                                'age uint8))])
  (set-field! preEncode struct (λ (val stream) (ref-set! val 'nameLength (length (ref val 'name)))))
  (send struct encode stream (mhasheq 'name "devon" 'age 21))
  (check-equal? (send stream dump)
                (+Buffer "\x05devon\x15")))


; todo: when pointer is ready
;    it 'should encode pointer data after structure', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer '\x05devon\x15\x08\x05hello'
;        done()
;
;      struct = new Struct
;        name: new StringT uint8
;        age: uint8
;        ptr: new Pointer uint8, new StringT uint8
;
;      struct.encode stream,
;        name: 'devon'
;        age: 21
;        ptr: 'hello'
;
;      stream.end()
(displayln 'warning:pointer-not-done)