#lang reader (submod "racket.rkt" reader)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/VersionedStruct.coffee
|#

;describe 'VersionedStruct', ->
;  describe 'decode', ->
;    it 'should get version from number type', ->

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))])

  (parameterize ([current-input-port (open-input-bytes #"\x00\x05devon\x15")])
    (check-equal? (dump (decode struct)) (mhasheq 'name "devon"
                                                  'age 21
                                                  'version 0)))

  (parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "\x01\x0adevon 👍\x15\x00"))])
    (check-equal? (dump (decode struct)) (mhasheq 'name "devon 👍"
                                                  'age 21
                                                  'version 1
                                                  'gender 0))))


;    it 'should throw for unknown version', ->

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))])

  (parameterize ([current-input-port (open-input-bytes #"\x05\x05devon\x15")])
    (check-exn exn:fail:contract? (λ () (decode struct)))))


;
;    it 'should support common header block', ->

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 'header (dictify 'age uint8
                                                  'alive uint8)
                                 0 (dictify 'name (+StringT uint8 'ascii))
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'gender uint8)))])

  (parameterize ([current-input-port (open-input-bytes #"\x00\x15\x01\x05devon")])
    (check-equal? (dump (decode struct)) (mhasheq 'name "devon"
                                                  'age 21
                                                  'alive 1
                                                  'version 0)))

  (parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "\x01\x15\x01\x0adevon 👍\x00"))])
    (check-equal? (dump (decode struct)) (mhasheq 'name "devon 👍"
                                                  'age 21
                                                  'version 1
                                                  'alive 1
                                                  'gender 0))))


;    it 'should support parent version key', ->

(let ([struct (+VersionedStruct 'version
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))])

  (parameterize ([current-input-port (open-input-bytes #"\x05devon\x15")])
    (check-equal? (dump (decode struct #:parent (mhash 'version 0))) (mhasheq 'name "devon"
                                                                              'age 21
                                                                              'version 0)))

  (parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "\x0adevon 👍\x15\x00"))])
    (check-equal? (dump (decode struct #:parent (mhash 'version 1))) (mhasheq 'name "devon 👍"
                                                                              'age 21
                                                                              'version 1
                                                                              'gender 0))))



;
;    it 'should support sub versioned structs', ->

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (+VersionedStruct uint8
                                                     (dictify
                                                      0 (dictify 'name (+StringT uint8))
                                                      1 (dictify 'name (+StringT uint8)
                                                                 'isDessert uint8)))))])

  (parameterize ([current-input-port (open-input-bytes #"\x00\x05devon\x15")])
    (check-equal? (dump (decode struct #:parent (mhash 'version 0))) (mhasheq 'name "devon"
                                                                              'age 21
                                                                              'version 0)))

  (parameterize ([current-input-port (open-input-bytes #"\x01\x00\x05pasta")])
    (check-equal? (dump (decode struct #:parent (mhash 'version 0))) (mhasheq 'name "pasta"
                                                                              'version 0)))

  (parameterize ([current-input-port (open-input-bytes #"\x01\x01\x09ice cream\x01")])
    (check-equal? (dump (decode struct #:parent (mhash 'version 0))) (mhasheq 'name "ice cream"
                                                                              'isDessert 1
                                                                              'version 1))))


;
;    it 'should support process hook', ->

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))])
  (set-field! post-decode struct (λ (o stream ctx) (ref-set! o 'processed "true") o))
  (parameterize ([current-input-port (open-input-bytes #"\x00\x05devon\x15")])
    (check-equal? (dump (decode struct)) (mhasheq 'name "devon"
                                                  'processed "true"
                                                  'age 21
                                                  'version 0))))


;
;  describe 'size', ->
;    it 'should compute the correct size', ->

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))])
  
  (check-equal? (size struct (mhasheq 'name "devon"
                                      'age 21
                                      'version 0)) 8)
  
  (check-equal? (size struct (mhasheq 'name "devon 👍"
                                      'gender 0
                                      'age 21
                                      'version 1)) 14))




;
;    it 'should throw for unknown version', ->

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))])
  
  (check-exn exn:fail:contract? (λ () (size struct (mhasheq 'name "devon"
                                                            'age 21
                                                            'version 5)))))


;
;    it 'should support common header block', ->

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 'header (dictify 'age uint8
                                                  'alive uint8)
                                 0 (dictify 'name (+StringT uint8 'ascii))
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'gender uint8)))])
  
  (check-equal? (size struct (mhasheq 'name "devon"
                                      'age 21
                                      'alive 1
                                      'version 0)) 9)
  
  (check-equal? (size struct (mhasheq 'name "devon 👍"
                                      'gender 0
                                      'age 21
                                      'alive 1
                                      'version 1)) 15))



;    it 'should compute the correct size with pointers', ->


(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'ptr (+Pointer uint8 (+StringT uint8)))))])
  
  (check-equal? (size struct (mhasheq 'name "devon"
                                      'age 21
                                      'version 1
                                      'ptr "hello")) 15))


;    
;    it 'should throw if no value is given', ->



(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))])
  
  (check-exn exn:fail:contract? (λ () (size struct))))



;  describe 'encode', ->
;    it 'should encode objects to buffers', (done) ->

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))]
      [port (open-output-bytes)])
  (encode struct (mhasheq 'name "devon"
                          'age 21
                          'version 0) port)
  (encode struct (mhasheq 'name "devon 👍"
                          'age 21
                          'gender 0
                          'version 1) port)
  (check-equal? (dump port) (string->bytes/utf-8 "\x00\x05devon\x15\x01\x0adevon 👍\x15\x00")))


;
;    it 'should throw for unknown version', ->

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))]
      [port (open-output-bytes)])
  (check-exn exn:fail:contract? (λ () (encode struct port (mhasheq 'name "devon"
                                                                   'age 21
                                                                   'version 5)))))



;    it 'should support common header block', (done) ->

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 'header (dictify 'age uint8
                                                  'alive uint8)
                                 0 (dictify 'name (+StringT uint8 'ascii))
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'gender uint8)))]
      [stream (open-output-bytes)])
  
  (encode struct (mhasheq 'name "devon"
                          'age 21
                          'alive 1
                          'version 0) stream)
  
  (encode struct (mhasheq 'name "devon 👍"
                          'gender 0
                          'age 21
                          'alive 1
                          'version 1) stream)
  
  (check-equal? (dump stream) (string->bytes/utf-8 "\x00\x15\x01\x05devon\x01\x15\x01\x0adevon 👍\x00")))



;    it 'should encode pointer data after structure', (done) ->

(let ([struct (+VersionedStruct uint8
                                (dictify 
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'ptr (+Pointer uint8 (+StringT uint8)))))]
      [stream (open-output-bytes)])
  (encode struct (mhasheq 'version 1
                          'name "devon"
                          'age 21
                          'ptr "hello") stream)

  (check-equal? (dump stream) (string->bytes/utf-8 "\x01\x05devon\x15\x09\x05hello")))




;    it 'should support preEncode hook', (done) ->

(let ([struct (+VersionedStruct uint8
                                (dictify
                                 0 (dictify 'name (+StringT uint8 'ascii)
                                            'age uint8)
                                 1 (dictify 'name (+StringT uint8 'utf8)
                                            'age uint8
                                            'gender uint8)))]
      [stream (open-output-bytes)])
  (set-field! pre-encode struct (λ (val port) (ref-set! val 'version (if (ref val 'gender) 1 0)) val))
  (encode struct (mhasheq 'name "devon"
                                      'age 21
                                      'version 0) stream)
  (encode struct (mhasheq 'name "devon 👍"
                                      'age 21
                                      'gender 0) stream)
  (check-equal? (dump stream) (string->bytes/utf-8 "\x00\x05devon\x15\x01\x0adevon 👍\x15\x00")))
