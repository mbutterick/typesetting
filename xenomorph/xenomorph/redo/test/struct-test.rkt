#lang debug racket/base
(require rackunit racket/dict
         "../helper.rkt"
         "../struct.rkt"
         "../string.rkt"
         "../pointer.rkt"
         "../number.rkt"
         sugar/unstable/dict)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Struct.coffee
|#

(test-case
 "decode into an object"
 (parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x15")])
   (check-equal?
    (dump (decode (+xstruct (dictify 'name (+xstring uint8)
                                     'age uint8))))
    '((name . "roxyb") (age . 21)))))

(test-case
 "decode with process hook"
 (parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x20")])
   (define struct (+xstruct (dictify 'name (+xstring uint8)
                                     'age uint8)))
   (set-post-decode! struct (λ (o . _) (dict-set! o 'canDrink (>= (dict-ref o 'age) 21)) o))
   (check-equal? (dump (decode struct))
                 '((name . "roxyb") (canDrink . #t) (age . 32)))))

(test-case
 "decode supports function keys"
 (parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x20")])
   (define struct (+xstruct (dictify 'name (+xstring uint8)
                                     'age uint8
                                     'canDrink (λ (o) (>= (dict-ref o 'age) 21)))))
   (check-equal? (dump (decode struct))
                 '((name . "roxyb") (canDrink . #t) (age . 32)))))

(test-case
 "compute the correct size"
 (check-equal? (size (+xstruct (dictify
                                'name (+xstring uint8)
                                'age uint8))
                     (hasheq 'name "roxyb" 'age 32)) 7))

(test-case
   "compute the correct size with pointers"
   (check-equal? (size (+xstruct (dictify
                                  'name (+xstring uint8)
                                  'age uint8
                                  'ptr (+xpointer uint8 (+xstring uint8))))
                       (mhash 'name "roxyb" 'age 21 'ptr "hello")) 14))

(test-case
 "get the correct size when no value is given"
 (check-equal? (size (+xstruct (dictify 'name (+xstring 4) 'age uint8))) 5))

(test-case
 "throw when getting non-fixed length size and no value is given"
 (check-exn exn:fail:contract? (λ () (size (+xstruct (dictify 'name (+xstring uint8)
                                                              'age uint8))))))

(test-case
 "encode objects to buffers"
 (parameterize ([current-input-port (open-input-bytes #"\x05roxyb\x15")])
   (check-equal? (dump (decode (+xstruct (dictify 'name (+xstring uint8)
                                                  'age uint8))))
                 '((name . "roxyb") (age . 21)))))

(test-case
 "support pre-encode hook"
 (parameterize ([current-output-port (open-output-bytes)])
   (define struct (+xstruct (dictify 'nameLength uint8
                                     'name (+xstring 'nameLength)
                                     'age uint8)))
   (set-pre-encode! struct (λ (val) (dict-set! val 'nameLength (string-length (dict-ref val 'name))) val))
   (encode struct (mhasheq 'name "roxyb" 'age 21))
   (check-equal? (dump (current-output-port)) #"\x05roxyb\x15")))

(test-case
   "encode pointer data after structure"
   (parameterize ([current-output-port (open-output-bytes)])
     (define struct (+xstruct (dictify 'name (+xstring uint8)
                                       'age uint8
                                       'ptr (+xpointer uint8 (+xstring uint8)))))
     (encode struct (hasheq 'name "roxyb" 'age 21 'ptr "hello"))
     (check-equal? (dump (current-output-port)) #"\x05roxyb\x15\x08\x05hello")))