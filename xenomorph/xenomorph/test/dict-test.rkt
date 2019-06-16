#lang racket/base
(require rackunit racket/dict
         racket/class
         "../base.rkt"
         "../dict.rkt"
         "../string.rkt"
         "../pointer.rkt"
         "../number.rkt"
         "../base.rkt"
         sugar/unstable/dict)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Struct.coffee
|#

(test-case
 "dict: decode into an object"
 (parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x15")])
   (check-equal?
    (decode (x:dict 'name (x:string #:length uint8) 'age uint8))
    (mhasheq 'name "roxyb" 'age 21))))

(test-case
 "dict: decode nested struct into an object"
 (parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x15\x05roxyb\x15")])
   (check-equal?
    (decode (x:dict 'name (x:string #:length uint8) 'age uint8
                    'nested (x:dict 'name (x:string #:length uint8) 'age uint8)))
    (mhasheq 'name "roxyb" 'age 21 'nested (mhasheq 'name "roxyb" 'age 21)))))

(test-case
 "dict: decode with process hook"
 (parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x20")])
   (define struct (x:dict #:post-decode (λ (o) (hash-set! o 'canDrink (>= (hash-ref o 'age) 21)) o)
                          'name (x:string #:length uint8) 'age uint8))
   (check-equal? (decode struct)
                 (mhasheq 'name "roxyb" 'age 32 'canDrink #t))))

(test-case
 "dict: decode supports function keys"
 (parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x20")])
   (define struct (x:dict 'name (x:string #:length uint8) 'age uint8 'canDrink (λ (o) (>= (hash-ref o 'age) 21))))
   (check-equal? (decode struct)
                 (mhasheq 'name "roxyb" 'age 32 'canDrink #t))))

(test-case
 "dict: compute the correct size"
 (check-equal? (send (x:dict 'name (x:string #:length uint8) 'age uint8)
                     x:size (hasheq 'name "roxyb" 'age 32)) 7))

(test-case
 "dict: compute the correct size with pointers"
 (check-equal? (send (x:dict 'name (x:string #:length uint8)
                             'age uint8
                             'ptr (x:pointer #:type uint8 #:dest-type (x:string #:length uint8)))
                     x:size
                     (mhash 'name "roxyb" 'age 21 'ptr "hello")) 14))

(test-case
 "dict: get the correct size when no value is given"
 (check-equal? (send (x:dict 'name (x:string 4) 'age uint8) x:size) 5))

(test-case
 "dict: throw when getting non-fixed length size and no value is given"
 (check-exn exn:fail:contract? (λ () (send (x:dict 'name (x:string #:length uint8) 'age uint8) x:size))))

(test-case
 "dict: encode objects to buffers"
 (parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x15")])
   (check-equal? (decode (x:dict 'name (x:string #:length uint8) 'age uint8))
                 (mhasheq 'name "roxyb" 'age 21))))

(test-case
 "dict: support pre-encode hook"
 (parameterize ([current-output-port (open-output-bytes)])
   (define struct (x:dict #:pre-encode (λ (val)
                                         (hash-set! val 'nameLength (string-length (hash-ref val 'name)))
                                         val)
                          'nameLength uint8
                          'name (x:string (λ (this) (hash-ref this 'nameLength)))
                          'age uint8))
   (encode struct (mhasheq 'name "roxyb" 'age 21))
   (check-equal? (get-output-bytes (current-output-port)) #"\x05roxyb\x15")))

(test-case
 "dict: encode pointer data after structure"
 (parameterize ([current-output-port (open-output-bytes)])
   (define struct (x:dict 'name (x:string #:length uint8)
                          'age uint8
                          'ptr (x:pointer uint8 #:dest-type (x:string #:length uint8))))
   (encode struct (hasheq 'name "roxyb" 'age 21 'ptr "hello"))
   (check-equal? (get-output-bytes (current-output-port)) #"\x05roxyb\x15\x08\x05hello")))