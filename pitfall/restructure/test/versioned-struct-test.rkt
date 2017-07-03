#lang restructure/test/racket

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/VersionedStruct.coffee
|#

;describe 'VersionedStruct', ->
;  describe 'decode', ->
;    it 'should get version from number type', ->
;      struct = new VersionedStruct uint8,
;        0:
;          name: new StringT uint8, 'ascii'
;          age: uint8
;        1:
;          name: new StringT uint8, 'utf8'
;          age: uint8
;          gender: uint8
;
;      stream = new DecodeStream new Buffer '\x00\x05devon\x15'
;      struct.decode(stream).should.deep.equal
;        version: 0
;        name: 'devon'
;        age: 21
;
;      stream = new DecodeStream new Buffer '\x01\x0adevon 👍\x15\x00', 'utf8'
;      struct.decode(stream).should.deep.equal
;        version: 1
;        name: 'devon 👍'
;        age: 21
;        gender: 0

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))])

  (let ([stream (+DecodeStream (+Buffer "\x00\x05devon\x15"))])
    (check-equal? (send (send struct decode stream) kv) (mhasheq 'name "devon"
                                                                 'age 21
                                                                 'version 0)))

  (let ([stream (+DecodeStream (+Buffer "\x01\x0adevon 👍\x15\x00"))])
    (check-equal? (send (send struct decode stream) kv) (mhasheq 'name "devon 👍"
                                                                 'age 21
                                                                 'version 1
                                                                 'gender 0))))



;
;    it 'should throw for unknown version', ->
;      struct = new VersionedStruct uint8,
;        0:
;          name: new StringT uint8, 'ascii'
;          age: uint8
;        1:
;          name: new StringT uint8, 'utf8'
;          age: uint8
;          gender: uint8
;
;      stream = new DecodeStream new Buffer '\x05\x05devon\x15'
;      should.throw ->
;        struct.decode(stream)


(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))])

  (let ([stream (+DecodeStream (+Buffer "\x05\x05devon\x15"))])
    (check-exn exn:fail:contract? (λ () (send struct decode stream)))))

;
;    it 'should support common header block', ->
;      struct = new VersionedStruct uint8,
;        header:
;          age: uint8
;          alive: uint8
;        0:
;          name: new StringT uint8, 'ascii'
;        1:
;          name: new StringT uint8, 'utf8'
;          gender: uint8
;
;      stream = new DecodeStream new Buffer '\x00\x15\x01\x05devon'
;      struct.decode(stream).should.deep.equal
;        version: 0
;        age: 21
;        alive: 1
;        name: 'devon'
;
;      stream = new DecodeStream new Buffer '\x01\x15\x01\x0adevon 👍\x00', 'utf8'
;      struct.decode(stream).should.deep.equal
;        version: 1
;        age: 21
;        alive: 1
;        name: 'devon 👍'
;        gender: 0


(let ([struct (+VersionedStruct uint8
                                (dictify
                                 'header (dictify 'age uint8
                                                  'alive uint8)
                                 0 (dictify 'name (+StringT uint8 'ascii))
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'gender uint8)))])

  (let ([stream (+DecodeStream (+Buffer "\x00\x15\x01\x05devon"))])
    (check-equal? (send (send struct decode stream) kv) (mhasheq 'name "devon"
                                                                 'age 21
                                                                 'alive 1
                                                                 'version 0)))

  (let ([stream (+DecodeStream (+Buffer "\x01\x15\x01\x0adevon 👍\x00"))])
    (check-equal? (send (send struct decode stream) kv) (mhasheq 'name "devon 👍"
                                                                 'age 21
                                                                 'version 1
                                                                 'alive 1
                                                                 'gender 0))))

;
;    it 'should support parent version key', ->
;      struct = new VersionedStruct 'version',
;        0:
;          name: new StringT uint8, 'ascii'
;          age: uint8
;        1:
;          name: new StringT uint8, 'utf8'
;          age: uint8
;          gender: uint8
;
;      stream = new DecodeStream new Buffer '\x05devon\x15'
;      struct.decode(stream, version: 0).should.deep.equal
;        version: 0
;        name: 'devon'
;        age: 21
;
;      stream = new DecodeStream new Buffer '\x0adevon 👍\x15\x00', 'utf8'
;      struct.decode(stream, version: 1).should.deep.equal
;        version: 1
;        name: 'devon 👍'
;        age: 21
;        gender: 0

(let ([struct (+VersionedStruct 'version
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))])

  (let ([stream (+DecodeStream (+Buffer "\x05devon\x15"))])
    (check-equal? (send (send struct decode stream (mhash 'version 0)) kv) (mhasheq 'name "devon"
                                                                                    'age 21
                                                                                    'version 0)))

  (let ([stream (+DecodeStream (+Buffer "\x0adevon 👍\x15\x00" 'utf8))])
    (check-equal? (send (send struct decode stream (mhash 'version 1)) kv) (mhasheq 'name "devon 👍"
                                                                                    'age 21
                                                                                    'version 1
                                                                                    'gender 0))))

;
;    it 'should support sub versioned structs', ->
;      struct = new VersionedStruct uint8,
;        0:
;          name: new StringT uint8, 'ascii'
;          age: uint8
;        1: new VersionedStruct uint8,
;          0:
;            name: new StringT uint8
;          1:
;            name: new StringT uint8
;            isDesert: uint8
;
;      stream = new DecodeStream new Buffer '\x00\x05devon\x15'
;      struct.decode(stream, version: 0).should.deep.equal
;        version: 0
;        name: 'devon'
;        age: 21
;
;      stream = new DecodeStream new Buffer '\x01\x00\x05pasta'
;      struct.decode(stream, version: 0).should.deep.equal
;        version: 0
;        name: 'pasta'
;
;      stream = new DecodeStream new Buffer '\x01\x01\x09ice cream\x01'
;      struct.decode(stream, version: 0).should.deep.equal
;        version: 1
;        name: 'ice cream'
;        isDesert: 1


(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (+VersionedStruct uint8
                                                     (dictify
                                                      0 (dictify 'name (+StringT uint8))
                                                      1 (dictify 'name (+StringT uint8)
                                                                 'isDessert uint8)))))])

  (let ([stream (+DecodeStream (+Buffer "\x00\x05devon\x15"))])
    (check-equal? (send (send struct decode stream (mhash 'version 0)) kv) (mhasheq 'name "devon"
                                                                                    'age 21
                                                                                    'version 0)))

  (let ([stream (+DecodeStream (+Buffer "\x01\x00\x05pasta"))])
    (check-equal? (send (send struct decode stream (mhash 'version 0)) kv) (mhasheq 'name "pasta"
                                                                                    'version 0)))

  (let ([stream (+DecodeStream (+Buffer "\x01\x01\x09ice cream\x01"))])
    (check-equal? (send (send struct decode stream (mhash 'version 0)) kv) (mhasheq 'name "ice cream"
                                                                                    'isDessert 1
                                                                                    'version 1))))


;
;    it 'should support process hook', ->
;      struct = new VersionedStruct uint8,
;        0:
;          name: new StringT uint8, 'ascii'
;          age: uint8
;        1:
;          name: new StringT uint8, 'utf8'
;          age: uint8
;          gender: uint8
;
;      struct.process = ->
;        @processed = true
;
;      stream = new DecodeStream new Buffer '\x00\x05devon\x15'
;      struct.decode(stream).should.deep.equal
;        version: 0
;        name: 'devon'
;        age: 21
;        processed: true


(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))])
  (set-field! process struct (λ (o stream ctx) (ref-set! o 'processed "true") o))
  (let ([stream (+DecodeStream (+Buffer "\x00\x05devon\x15"))])
    (check-equal? (send (send struct decode stream) kv) (mhasheq 'name "devon"
                                                                 'processed "true"
                                                                 'age 21
                                                                 'version 0))))

;
;  describe 'size', ->
;    it 'should compute the correct size', ->
;      struct = new VersionedStruct uint8,
;        0:
;          name: new StringT uint8, 'ascii'
;          age: uint8
;        1:
;          name: new StringT uint8, 'utf8'
;          age: uint8
;          gender: uint8
;
;      size = struct.size
;        version: 0
;        name: 'devon'
;        age: 21
;
;      size.should.equal 8
;
;      size = struct.size
;        version: 1
;        name: 'devon 👍'
;        age: 21
;        gender: 0
;
;      size.should.equal 14

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))])
  
  (check-equal? (send struct size (mhasheq 'name "devon"
                                           'age 21
                                           'version 0)) 8)
  
  (check-equal? (send struct size (mhasheq 'name "devon 👍"
                                           'gender 0
                                           'age 21
                                           'version 1)) 14))


;
;    it 'should throw for unknown version', ->
;      struct = new VersionedStruct uint8,
;        0:
;          name: new StringT uint8, 'ascii'
;          age: uint8
;        1:
;          name: new StringT uint8, 'utf8'
;          age: uint8
;          gender: uint8
;
;      should.throw ->
;        struct.size
;          version: 5
;          name: 'devon'
;          age: 21

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))])
  
  (check-exn exn:fail:contract? (λ () (send struct size (mhasheq 'name "devon"
                                                                 'age 21
                                                                 'version 5)))))


;
;    it 'should support common header block', ->
;      struct = new VersionedStruct uint8,
;        header:
;          age: uint8
;          alive: uint8
;        0:
;          name: new StringT uint8, 'ascii'
;        1:
;          name: new StringT uint8, 'utf8'
;          gender: uint8
;
;      size = struct.size
;        version: 0
;        age: 21
;        alive: 1
;        name: 'devon'
;
;      size.should.equal 9
;
;      size = struct.size
;        version: 1
;        age: 21
;        alive: 1
;        name: 'devon 👍'
;        gender: 0
;
;      size.should.equal 15

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 'header (dictify 'age uint8
                                                  'alive uint8)
                                 0 (dictify 'name (+StringT uint8 'ascii))
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'gender uint8)))])
  
  (check-equal? (send struct size (mhasheq 'name "devon"
                                           'age 21
                                           'alive 1
                                           'version 0)) 9)
  
  (check-equal? (send struct size (mhasheq 'name "devon 👍"
                                           'gender 0
                                           'age 21
                                           'alive 1
                                           'version 1)) 15))


;    it 'should compute the correct size with pointers', ->
;      struct = new VersionedStruct uint8,
;        0:
;          name: new StringT uint8, 'ascii'
;          age: uint8
;        1:
;          name: new StringT uint8, 'utf8'
;          age: uint8
;          ptr: new Pointer uint8, new StringT uint8
;
;      size = struct.size
;        version: 1
;        name: 'devon'
;        age: 21
;        ptr: 'hello'
;
;      size.should.equal 15



(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'ptr (+Pointer uint8 (+StringT uint8)))))])
  
  (check-equal? (send struct size (mhasheq 'name "devon"
                                           'age 21
                                           'version 1
                                           'ptr "hello")) 15))


;    
;    it 'should throw if no value is given', ->
;      struct = new VersionedStruct uint8,
;        0:
;          name: new StringT 4, 'ascii'
;          age: uint8
;        1:
;          name: new StringT 4, 'utf8'
;          age: uint8
;          gender: uint8
;
;      should.throw ->
;        struct.size()
;      , /not a fixed size/i


(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))])
  
  (check-exn exn:fail:contract? (λ () (send struct size))))

;
;  describe 'encode', ->
;    it 'should encode objects to buffers', (done) ->
;      struct = new VersionedStruct uint8,
;        0:
;          name: new StringT uint8, 'ascii'
;          age: uint8
;        1:
;          name: new StringT uint8, 'utf8'
;          age: uint8
;          gender: uint8
;
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer '\x00\x05devon\x15\x01\x0adevon 👍\x15\x00', 'utf8'
;        done()
;
;      struct.encode stream,
;        version: 0
;        name: 'devon'
;        age: 21
;
;      struct.encode stream,
;        version: 1
;        name: 'devon 👍'
;        age: 21
;        gender: 0
;
;      stream.end()

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))]
      [stream (+EncodeStream)])
  (send struct encode stream (mhasheq 'name "devon"
                                      'age 21
                                      'version 0))
  (send struct encode stream (mhasheq 'name "devon 👍"
                                      'age 21
                                      'gender 0
                                      'version 1))
  (check-equal? (send stream dump) (+Buffer "\x00\x05devon\x15\x01\x0adevon 👍\x15\x00" 'utf8)))


;
;    it 'should throw for unknown version', ->
;      struct = new VersionedStruct uint8,
;        0:
;          name: new StringT uint8, 'ascii'
;          age: uint8
;        1:
;          name: new StringT uint8, 'utf8'
;          age: uint8
;          gender: uint8
;
;      stream = new EncodeStream
;      should.throw ->
;        struct.encode stream,
;          version: 5
;          name: 'devon'
;          age: 21

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))]
      [stream (+EncodeStream)])
  (check-exn exn:fail:contract? (λ () (send struct encode stream (mhasheq 'name "devon"
                                                                          'age 21
                                                                          'version 5)))))

;
;    it 'should support common header block', (done) ->
;      struct = new VersionedStruct uint8,
;        header:
;          age: uint8
;          alive: uint8
;        0:
;          name: new StringT uint8, 'ascii'
;        1:
;          name: new StringT uint8, 'utf8'
;          gender: uint8
;
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer '\x00\x15\x01\x05devon\x01\x15\x01\x0adevon 👍\x00', 'utf8'
;        done()
;
;      struct.encode stream,
;        version: 0
;        age: 21
;        alive: 1
;        name: 'devon'
;
;      struct.encode stream,
;        version: 1
;        age: 21
;        alive: 1
;        name: 'devon 👍'
;        gender: 0
;
;      stream.end()


(let ([struct (+VersionedStruct uint8
                                (dictify
                                 'header (dictify 'age uint8
                                                  'alive uint8)
                                 0 (dictify 'name (+StringT uint8 'ascii))
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'gender uint8)))]
      [stream (+EncodeStream)])
  
  (send struct encode stream (mhasheq 'name "devon"
                                      'age 21
                                      'alive 1
                                      'version 0))
  
  (send struct encode stream (mhasheq 'name "devon 👍"
                                      'gender 0
                                      'age 21
                                      'alive 1
                                      'version 1))
  
  (check-equal? (send stream dump) (+Buffer "\x00\x15\x01\x05devon\x01\x15\x01\x0adevon 👍\x00" 'utf8)))



;    it 'should encode pointer data after structure', (done) ->
;      struct = new VersionedStruct uint8,
;        0:
;          name: new StringT uint8, 'ascii'
;          age: uint8
;        1:
;          name: new StringT uint8, 'utf8'
;          age: uint8
;          ptr: new Pointer uint8, new StringT uint8

;
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer '\x01\x05devon\x15\x09\x05hello', 'utf8'
;        done()
;
;      struct.encode stream,
;        version: 1
;        name: 'devon'
;        age: 21
;        ptr: 'hello'
;
;      stream.end()

(let ([struct (+VersionedStruct uint8
                                (dictify 
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'ptr (+Pointer uint8 (+StringT uint8)))))]
      [stream (+EncodeStream)])
  (send struct encode stream (mhasheq 'version 1
                                      'name "devon"
                                      'age 21
                                      'ptr "hello"))

  (check-equal? (send stream dump) (+Buffer "\x01\x05devon\x15\x09\x05hello" 'utf8)))


;
;    it 'should support preEncode hook', (done) ->
;      struct = new VersionedStruct uint8,
;        0:
;          name: new StringT uint8, 'ascii'
;          age: uint8
;        1:
;          name: new StringT uint8, 'utf8'
;          age: uint8
;          gender: uint8
;
;      struct.preEncode = ->
;        @version = if @gender? then 1 else 0
;
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer '\x00\x05devon\x15\x01\x0adevon 👍\x15\x00', 'utf8'
;        done()
;
;      struct.encode stream,
;        name: 'devon'
;        age: 21
;
;      struct.encode stream,
;        name: 'devon 👍'
;        age: 21
;        gender: 0
;
;      stream.end()


(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))]
      [stream (+EncodeStream)])
  (set-field! preEncode struct (λ (val stream) (ref-set! val 'version (if (ref val 'gender) 1 0))))
  (send struct encode stream (mhasheq 'name "devon"
                                      'age 21
                                      'version 0))
  (send struct encode stream (mhasheq 'name "devon 👍"
                                      'age 21
                                      'gender 0))
  (check-equal? (send stream dump) (+Buffer "\x00\x05devon\x15\x01\x0adevon 👍\x15\x00" 'utf8)))