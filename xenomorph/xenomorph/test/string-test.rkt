#lang racket/base
(require rackunit
         racket/class
         "../base.rkt"
         "../string.rkt"
         "../number.rkt"
         "../base.rkt"
         sugar/unstable/dict)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/String.coffee
|#

(test-case
 "string: decode fixed length"
 (parameterize ([current-input-port (open-input-bytes #"testing")])
   (check-equal? (decode (x:string 7)) "testing")))

(test-case
 "string: decode fixed length with post-decode"
 (parameterize ([current-input-port (open-input-bytes #"testing")])
   (define xs (x:string 7 #:post-decode (λ (val) "ring a ding")))
   (check-equal? (decode xs) "ring a ding")))

(test-case
 "string: decode length from parent key"
 (parameterize ([current-input-port (open-input-bytes #"testing")])
   (check-equal? (decode (x:string 'len) (current-input-port) #:parent (mhash 'len 7)) "testing")))

(test-case
 "string: decode length as number before string"
 (parameterize ([current-input-port (open-input-bytes #"\x07testing")])
   (check-equal? (decode (x:string uint8) (current-input-port) #:parent (mhash 'len 7)) "testing")))

(test-case
 "string: decode utf8"
 (parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "🍻"))])
   (check-equal? (decode (x:string 4 'utf8)) "🍻")))

(test-case
 "string: decode encoding computed from function"
 (parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "🍻"))])
   (check-equal? (decode (x:string 4 (λ _ 'utf8))) "🍻")))

(test-case
 "string: decode null-terminated string and read past terminator"
 (parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "🍻\x00"))])
   (check-equal? (decode (x:string #f 'utf8)) "🍻")
   (check-equal? (pos (current-input-port)) 5)))

(test-case
 "string: decode remainder of buffer when null-byte missing"
 (parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "🍻"))])
   (check-equal? (decode (x:string #f 'utf8)) "🍻")))

(test-case
 "string: size should use string length"
 (check-equal? (size (x:string 7) "testing") 7))

(test-case
 "string: size should use correct encoding"
 (check-equal? (size (x:string 10 'utf8) "🍻") 4))

(test-case
 "string: size should use encoding from function"
 (check-equal? (size (x:string 10 (λ _ 'utf8)) "🍻") 4))

(test-case
 "string: should add size of length field before string"
 (check-equal? (size (x:string uint8 'utf8) "🍻") 5))

; todo: it "should work with utf16be encoding"

(test-case
 "string: size should take null-byte into account"
 (check-equal? (size (x:string #f 'utf8) "🍻") 5))

(test-case
 "string: size should use defined length if no value given"
 (check-equal? (size (x:string 10)) 10))

(test-case
 "string: encode using string length"
 (parameterize ([current-output-port (open-output-bytes)])
   (encode (x:string 7) "testing")
   (check-equal? (get-output-bytes (current-output-port)) #"testing")))

(test-case
 "string: encode using string length and pre-encode"
 (parameterize ([current-output-port (open-output-bytes)])
   (define xs (x:string 7 #:pre-encode (λ (val) (list->string (reverse (string->list val))))))
   (encode xs "testing")
   (check-equal? (get-output-bytes (current-output-port)) #"gnitset")))

(test-case
 "string: encode length as number before string"
 (parameterize ([current-output-port (open-output-bytes)])
   (encode (x:string uint8) "testing")
   (check-equal? (get-output-bytes (current-output-port)) #"\x07testing")))

(test-case
 "string: encode length as number before string utf8"
 (parameterize ([current-output-port (open-output-bytes)])
   (encode (x:string uint8 'utf8) "testing 😜")
   (check-equal? (get-output-bytes (current-output-port)) (string->bytes/utf-8 "\x0ctesting 😜"))))

(test-case
 "string: encode utf8"
 (parameterize ([current-output-port (open-output-bytes)])
   (encode (x:string 4 'utf8) "🍻" )
   (check-equal? (get-output-bytes (current-output-port)) (string->bytes/utf-8 "🍻"))))

(test-case
 "string: encode encoding computed from function"
 (parameterize ([current-output-port (open-output-bytes)])
   (encode (x:string 4 (λ _ 'utf8)) "🍻")
   (check-equal? (get-output-bytes (current-output-port)) (string->bytes/utf-8 "🍻"))))

(test-case
 "string: encode null-terminated string"
 (parameterize ([current-output-port (open-output-bytes)])
   (encode (x:string #f 'utf8) "🍻"  )
   (check-equal? (get-output-bytes (current-output-port)) (string->bytes/utf-8 "🍻\x00"))))