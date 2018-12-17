#lang debug racket/base
(require rackunit racket/dict
         racket/class
         "../helper.rkt"
         "../struct.rkt"
         "../string.rkt"
         "../pointer.rkt"
         "../number.rkt"
         "../generic.rkt"
         sugar/unstable/dict)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Struct.coffee
|#

(test-case
 "struct: decode into an object"
 (parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x15")])
   (check-equal?
    (decode (+xstruct 'name (+xstring #:length uint8) 'age uint8))
    (mhasheq 'name "roxyb" 'age 21))))

(test-case
 "struct: decode with process hook"
 (parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x20")])
   (define struct (+xstruct #:post-decode (λ (o) (dict-set! o 'canDrink (>= (dict-ref o 'age) 21)) o)
                   'name (+xstring #:length uint8) 'age uint8))
   (check-equal? (decode struct)
                 (mhasheq 'name "roxyb" 'age 32 'canDrink #t))))

(test-case
 "struct: decode supports function keys"
 (parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x20")])
   (define struct (+xstruct 'name (+xstring #:length uint8) 'age uint8 'canDrink (λ (o) (>= (dict-ref o 'age) 21))))
   (check-equal? (decode struct)
                 (mhasheq 'name "roxyb" 'age 32 'canDrink #t))))

(test-case
 "struct: compute the correct size"
 (check-equal? (size (+xstruct 'name (+xstring #:length uint8) 'age uint8)
                     (hasheq 'name "roxyb" 'age 32)) 7))

(test-case
 "struct: compute the correct size with pointers"
 (check-equal? (size (+xstruct 'name (+xstring #:length uint8)
                               'age uint8
                               'ptr (+xpointer #:type (+xstring #:length uint8)))
                     (mhash 'name "roxyb" 'age 21 'ptr "hello")) 14))

(test-case
 "struct: get the correct size when no value is given"
 (check-equal? (size (+xstruct 'name (+xstring 4) 'age uint8)) 5))

(test-case
 "struct: throw when getting non-fixed length size and no value is given"
 (check-exn exn:fail:contract? (λ () (size (+xstruct 'name (+xstring #:length uint8) 'age uint8)))))

(test-case
 "struct: encode objects to buffers"
 (parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x15")])
   (check-equal? (decode (+xstruct 'name (+xstring #:length uint8) 'age uint8))
                 (mhasheq 'name "roxyb" 'age 21))))

(test-case
 "struct: support pre-encode hook"
 (parameterize ([current-output-port (open-output-bytes)])
   (define struct (+xstruct #:pre-encode (λ (val)
                         (dict-set! val 'nameLength (string-length (dict-ref val 'name))) val)
                            'nameLength uint8
                            'name (+xstring 'nameLength)
                            'age uint8))
   (encode struct (mhasheq 'name "roxyb" 'age 21))
   (check-equal? (get-output-bytes (current-output-port)) #"\x05roxyb\x15")))

(test-case
 "struct: encode pointer data after structure"
 (parameterize ([current-output-port (open-output-bytes)])
   (define struct (+xstruct 'name (+xstring #:length uint8)
                            'age uint8
                            'ptr (+xpointer #:type (+xstring #:length uint8))))
   (encode struct (hasheq 'name "roxyb" 'age 21 'ptr "hello"))
   (check-equal? (get-output-bytes (current-output-port)) #"\x05roxyb\x15\x08\x05hello")))