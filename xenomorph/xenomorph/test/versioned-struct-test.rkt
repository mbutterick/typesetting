#lang racket/base
(require rackunit
         racket/dict
         sugar/unstable/dict
         "../helper.rkt"
         "../number.rkt"
         "../string.rkt"
         "../pointer.rkt"
         "../struct.rkt"
         "../generic.rkt"
         "../versioned-struct.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/VersionedStruct.coffee
|#

(test-case
 "versioned struct: decode should get version from number type"
 (let ([vstruct (x:versioned-struct uint8
                                    (dictify
                                     0 (dictify 'name (x:string #:length uint8 #:encoding 'ascii)
                                                'age uint8)
                                     1 (x:struct 'name (x:string #:length uint8 #:encoding 'utf8)
                                                'age uint8
                                                'gender uint8)))])
   (parameterize ([current-input-port (open-input-bytes #"\x00\x05roxyb\x15")])
     (check-equal? (decode vstruct) (mhasheq 'name "roxyb" 'age 21 x:version-key 0)))
   (parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "\x01\x0aroxyb 🤘\x15\x00"))])
     (check-equal? (decode vstruct) (mhasheq 'name "roxyb 🤘" 'age 21 x:version-key 1 'gender 0)))))

(test-case
 "versioned struct: decode should throw for unknown version"
 (let ([vstruct (x:versioned-struct uint8
                                    (dictify
                                     0 (dictify 'name (x:string #:length uint8 #:encoding 'ascii)
                                                'age uint8)
                                     1 (x:struct 'name (x:string #:length uint8 #:encoding 'utf8)
                                                'age uint8
                                                'gender uint8)))])
   (parameterize ([current-input-port (open-input-bytes #"\x05\x05roxyb\x15")])
     (check-exn exn:fail:contract? (λ () (decode vstruct))))))

(test-case
 "versioned struct: decode should support common header block"
 (let ([vstruct (x:versioned-struct uint8
                                    (dictify
                                     'header (dictify 'age uint8
                                                      'alive uint8)
                                     0 (dictify 'name (x:string #:length uint8 #:encoding 'ascii))
                                     1 (x:struct 'name (x:string #:length uint8 #:encoding 'utf8)
                                                'gender uint8)))])
   (parameterize ([current-input-port (open-input-bytes #"\x00\x15\x01\x05roxyb")])
     (check-equal? (decode vstruct) (mhasheq 'name "roxyb"
                                                 'age 21
                                                 'alive 1
                                                 x:version-key 0)))
   (parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "\x01\x15\x01\x0aroxyb 🤘\x00"))])
     (check-equal? (decode vstruct) (mhasheq 'name "roxyb 🤘"
                                                 'age 21
                                                 x:version-key 1
                                                 'alive 1
                                                 'gender 0)))))

(test-case
 "versioned struct: decode should support parent version key"
 (let ([vstruct (x:versioned-struct x:version-key
                                    (dictify
                                     0 (dictify 'name (x:string #:length uint8 #:encoding 'ascii)
                                                'age uint8)
                                     1 (x:struct 'name (x:string #:length uint8 #:encoding 'utf8)
                                                'age uint8
                                                'gender uint8)))])
   (parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x15")])
     (check-equal? (decode vstruct #:parent (mhash x:version-key 0))
                   (mhasheq 'name "roxyb" 'age 21 x:version-key 0)))
   (parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "\x0aroxyb 🤘\x15\x00"))])
     (check-equal? (decode vstruct #:parent (mhash x:version-key 1))
                   (mhasheq 'name "roxyb 🤘"  'age 21 x:version-key 1 'gender 0)))))

(test-case
 "versioned struct: decode should support sub versioned structs"
 (let ([vstruct (x:versioned-struct uint8
                                    (dictify
                                     0 (dictify 'name (x:string #:length uint8 #:encoding 'ascii)
                                                'age uint8)
                                     1 (x:versioned-struct uint8
                                                           (dictify
                                                            0 (dictify 'name (x:string uint8))
                                                            1 (dictify 'name (x:string uint8)
                                                                       'isDessert uint8)))))])
   (parameterize ([current-input-port (open-input-bytes #"\x00\x05roxyb\x15")])
     (check-equal? (decode vstruct #:parent (mhash x:version-key 0))
                   (mhasheq 'name "roxyb" 'age 21 x:version-key 0)))
   (parameterize ([current-input-port (open-input-bytes #"\x01\x00\x05pasta")])
     (check-equal? (decode vstruct #:parent (mhash x:version-key 0))
                   (mhasheq 'name "pasta" x:version-key 0)))
   (parameterize ([current-input-port (open-input-bytes #"\x01\x01\x09ice cream\x01")])
     (check-equal? (decode vstruct #:parent (mhash x:version-key 0))
                   (mhasheq 'name "ice cream" 'isDessert 1 x:version-key 1)))))

(test-case
 "versioned struct: decode should support process hook"
 (let ([vstruct (x:versioned-struct #:post-decode (λ (val) (hash-set! val 'processed "true") val)
                                    uint8
                                    (dictify
                                     0 (dictify 'name (x:string #:length uint8 #:encoding 'ascii)
                                                'age uint8)
                                     1 (x:struct 'name (x:string #:length uint8 #:encoding 'utf8)
                                                'age uint8
                                                'gender uint8)))])
   (parameterize ([current-input-port (open-input-bytes #"\x00\x05roxyb\x15")])
     (check-equal? (decode vstruct)
                   (mhasheq 'name "roxyb" 'processed "true" 'age 21 x:version-key 0)))))

(test-case
 "versioned struct: size should compute the correct size"
 (let ([vstruct (x:versioned-struct uint8
                                    (dictify
                                     0 (dictify 'name (x:string #:length uint8 #:encoding 'ascii)
                                                'age uint8)
                                     1 (x:struct 'name (x:string #:length uint8 #:encoding 'utf8)
                                                'age uint8
                                                'gender uint8)))])
   (check-equal? (size vstruct (mhasheq 'name "roxyb"
                                        'age 21
                                        x:version-key 0)) 8)
   (check-equal? (size vstruct (mhasheq 'name "roxyb 🤘"
                                        'gender 0
                                        'age 21
                                        x:version-key 1)) 14)))

(test-case
 "versioned struct: size should throw for unknown version"
 (let ([vstruct (x:versioned-struct uint8
                                    (dictify
                                     0 (dictify 'name (x:string #:length uint8 #:encoding 'ascii)
                                                'age uint8)
                                     1 (x:struct 'name (x:string #:length uint8 #:encoding 'utf8)
                                                'age uint8
                                                'gender uint8)))])
   (check-exn exn:fail:contract? (λ () (size vstruct (mhasheq 'name "roxyb" 'age 21 x:version-key 5))))))

(test-case
 "versioned struct: size should support common header block"
 (let ([struct (x:versioned-struct uint8
                                   (dictify
                                    'header (dictify 'age uint8
                                                     'alive uint8)
                                    0 (dictify 'name (x:string #:length uint8 #:encoding 'ascii))
                                    1 (x:struct 'name (x:string #:length uint8 #:encoding 'utf8)
                                               'gender uint8)))])
   (check-equal? (size struct (mhasheq 'name "roxyb" 'age 21 'alive 1 x:version-key 0)) 9)
   (check-equal? (size struct (mhasheq 'name "roxyb 🤘" 'gender 0 'age 21 'alive 1 x:version-key 1)) 15)))

(test-case
 "versioned struct: size should compute the correct size with pointers"
 (let ([vstruct (x:versioned-struct uint8
                                    (dictify
                                     0 (dictify 'name (x:string #:length uint8 #:encoding 'ascii)
                                                'age uint8)
                                     1 (x:struct 'name (x:string #:length uint8 #:encoding 'utf8)
                                                'age uint8
                                                'ptr (x:pointer #:offset-type uint8
                                                                #:type (x:string uint8)))))])
   (check-equal? (size vstruct (mhasheq 'name "roxyb"
                                        'age 21
                                        x:version-key 1
                                        'ptr "hello")) 15)))

(test-case
 "versioned struct: size should throw if no value is given"
 (let ([vstruct (x:versioned-struct uint8
                                    (dictify
                                     0 (dictify 'name (x:string #:length uint8 #:encoding 'ascii)
                                                'age uint8)
                                     1 (x:struct 'name (x:string #:length uint8 #:encoding 'utf8)
                                                'age uint8
                                                'gender uint8)))])
   (check-exn exn:fail:contract? (λ () (size vstruct)))))

(test-case
 "versioned struct: encode should encode objects to buffers"
 (let ([vstruct (x:versioned-struct uint8
                                    (dictify
                                     0 (dictify 'name (x:string #:length uint8 #:encoding 'ascii)
                                                'age uint8)
                                     1 (x:struct 'name (x:string #:length uint8 #:encoding 'utf8)
                                                'age uint8
                                                'gender uint8)))]
       [op (open-output-bytes)])
   (encode vstruct (mhasheq 'name "roxyb" 'age 21 x:version-key 0) op)
   (encode vstruct (mhasheq 'name "roxyb 🤘" 'age 21 'gender 0 x:version-key 1) op)
   (check-equal? (get-output-bytes op) (string->bytes/utf-8 "\x00\x05roxyb\x15\x01\x0aroxyb 🤘\x15\x00"))))

(test-case
 "versioned struct: encode should throw for unknown version"
 (let ([vstruct (x:versioned-struct uint8
                                    (dictify
                                     0 (dictify 'name (x:string #:length uint8 #:encoding 'ascii)
                                                'age uint8)
                                     1 (x:struct 'name (x:string #:length uint8 #:encoding 'utf8)
                                                'age uint8
                                                'gender uint8)))]
       [op (open-output-bytes)])
   (check-exn exn:fail:contract? (λ () (encode vstruct op (mhasheq 'name "roxyb" 'age 21 x:version-key 5))))))

(test-case
 "versioned struct: encode should support common header block"
 (let ([vstruct (x:versioned-struct uint8
                                    (dictify
                                     'header (dictify 'age uint8
                                                      'alive uint8)
                                     0 (dictify 'name (x:string #:length uint8 #:encoding 'ascii))
                                     1 (x:struct 'name (x:string #:length uint8 #:encoding 'utf8)
                                                'gender uint8)))]
       [op (open-output-bytes)])
   (encode vstruct (mhasheq 'name "roxyb" 'age 21 'alive 1 x:version-key 0) op)
   (encode vstruct (mhasheq 'name "roxyb 🤘" 'gender 0 'age 21 'alive 1 x:version-key 1) op)
   (check-equal? (get-output-bytes op) (string->bytes/utf-8 "\x00\x15\x01\x05roxyb\x01\x15\x01\x0aroxyb 🤘\x00"))))

(test-case
 "versioned struct: encode should encode pointer data after structure"
 (let ([vstruct (x:versioned-struct uint8
                                    (dictify 
                                     0 (dictify 'name (x:string #:length uint8 #:encoding 'ascii)
                                                'age uint8)
                                     1 (x:struct 'name (x:string #:length uint8 #:encoding 'utf8)
                                                'age uint8
                                                'ptr (x:pointer #:offset-type uint8
                                                                #:type (x:string uint8)))))]
       [op (open-output-bytes)])
   (encode vstruct (mhasheq x:version-key 1 'name "roxyb" 'age 21 'ptr "hello") op)

   (check-equal? (get-output-bytes op) (string->bytes/utf-8 "\x01\x05roxyb\x15\x09\x05hello"))))

#;(test-case
 "versioned struct: encode should support preEncode hook"
 (let ([vstruct (x:versioned-struct uint8
                                    (dictify
                                     0 (dictify 'name (x:string #:length uint8 #:encoding 'ascii)
                                                'age uint8)
                                     1 (x:struct 'name (x:string #:length uint8 #:encoding 'utf8)
                                                'age uint8
                                                'gender uint8)))]
       [op (open-output-bytes)])
   (set-pre-encode! vstruct (λ (val) (hash-set! val x:version-key (if (hash-ref val 'gender #f) 1 0)) val))
   (encode vstruct (mhasheq 'name "roxyb" 'age 21 x:version-key 0) op)
   (encode vstruct (mhasheq 'name "roxyb 🤘" 'age 21 'gender 0) op)
   (check-equal? (get-output-bytes op) (string->bytes/utf-8 "\x00\x05roxyb\x15\x01\x0aroxyb 🤘\x15\x00"))))