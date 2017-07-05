#lang reader (submod "racket.rkt" reader)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Struct.coffee
|#


;describe 'Struct', ->
;  describe 'decode', ->
;    it 'should decode into an object', ->

(parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x15")])
  (check-equal?
   (dump (decode (+Struct (dictify 'name (+StringT uint8)
                                   'age uint8))))
   (mhasheq 'name "roxyb" 'age 21)))



;    it 'should support process hook', ->

(parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x20")])
  (define struct (+Struct (dictify 'name (+StringT uint8)
                                   'age uint8)))
  (set-field! post-decode struct (λ (o . _) (ref-set! o 'canDrink (>= (· o age) 21)) o))
  (check-equal? (dump (decode struct))
                (mhasheq 'name "roxyb" 'age 32 'canDrink #t)))



;    it 'should support function keys', ->

(parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x20")])
  (define struct (+Struct (dictify 'name (+StringT uint8)
                                   'age uint8
                                   'canDrink (λ (o) (>= (ref o 'age) 21)))))
  (check-equal? (dump (decode struct))
                (mhasheq 'name "roxyb" 'age 32 'canDrink #t)))




;
;  describe 'size', ->
;    it 'should compute the correct size', ->

(check-equal? (size (+Struct (dictify
                              'name (+StringT uint8)
                              'age uint8))
                    (hasheq 'name "roxyb" 'age 32)) 7)



;    it 'should compute the correct size with pointers', ->

(check-equal? (size (+Struct (dictify
                              'name (+StringT uint8)
                              'age uint8
                              'ptr (+Pointer uint8 (+StringT uint8))))
                    (mhash 'name "roxyb" 'age 21 'ptr "hello")) 14)


;    it 'should get the correct size when no value is given', ->

(check-equal? (size (+Struct (dictify
                              'name (+StringT 4)
                              'age uint8))) 5)
      
;    it 'should throw when getting non-fixed length size and no value is given', ->

(check-exn exn:fail:contract? (λ () (size (+Struct (dictify 'name (+StringT uint8)
                                                            'age uint8)))))



;      
;  describe 'encode', ->
;    it 'should encode objects to buffers', (done) ->
;      stream = new EncodeStream
;      stream.pipe concat (buf) ->
;        buf.should.deep.equal new Buffer '\x05roxyb\x15'
;        done()
;
;      struct = new Struct
;        name: new StringT uint8
;        age: uint8
;
;      struct.encode stream,
;        name: 'roxyb'
;        age: 21
;
;      stream.end()

(parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x15")])
  (check-equal? (dump (decode (+Struct (dictify 'name (+StringT uint8)
                                                'age uint8))))
                (mhasheq 'name "roxyb" 'age 21)))


;    it 'should support preEncode hook', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (define struct (+Struct (dictify 'nameLength uint8
                                   'name (+StringT 'nameLength)
                                   'age uint8)))
  (set-field! pre-encode struct (λ (val port) (ref-set! val 'nameLength (length (ref val 'name))) val))
  (encode struct (mhasheq 'name "roxyb" 'age 21))
  (check-equal? (dump (current-output-port)) #"\x05roxyb\x15"))


;    it 'should encode pointer data after structure', (done) ->

(parameterize ([current-output-port (open-output-bytes)])
  (define struct (+Struct (dictify 'name (+StringT uint8)
                                   'age uint8
                                   'ptr (+Pointer uint8 (+StringT uint8)))))
  (encode struct (mhasheq 'name "roxyb" 'age 21 'ptr "hello"))
  (check-equal? (dump (current-output-port)) #"\x05roxyb\x15\x08\x05hello"))

