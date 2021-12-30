#lang racket/base
(require rackunit
         racket/class
         "../base.rkt"
         "../symbol.rkt"
         "../number.rkt"
         "../base.rkt"
         sugar/unstable/dict)


(test-case
 "symbol: decode fixed length"
 (parameterize ([current-input-port (open-input-bytes #"testing")])
   (check-equal? (decode (x:symbol 7)) 'testing)))

(test-case
 "symbol: decode fixed length with post-decode"
 (parameterize ([current-input-port (open-input-bytes #"testing")])
   (define xs (x:symbol 7 #:post-decode (λ (val) '|ring a ding|)))
   (check-equal? (decode xs) '|ring a ding|)))

(test-case
 "symbol: decode length from parent key"
 (parameterize ([current-input-port (open-input-bytes #"testing")])
   (check-equal? (decode (x:symbol (λ (p) (hash-ref p 'len))) (current-input-port) #:parent (mhash 'len 7)) 'testing)))

(test-case
 "symbol: decode length as number before symbol"
 (parameterize ([current-input-port (open-input-bytes #"\x07testing")])
   (check-equal? (decode (x:symbol uint8) (current-input-port) #:parent (mhash 'len 7)) 'testing)))

(test-case
 "symbol: decode utf8"
 (parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "🍻"))])
   (check-equal? (decode (x:symbol 4 'utf8)) '🍻)))

(test-case
 "symbol: decode encoding computed from function"
 (parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "🍻"))])
   (check-equal? (decode (x:symbol 4 (λ _ 'utf8))) '🍻)))

(test-case
 "symbol: decode null-terminated symbol and read past terminator"
 (parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "🍻\x00"))])
   (check-equal? (decode (x:symbol #f 'utf8)) '🍻)
   (check-equal? (pos (current-input-port)) 5)))

(test-case
 "symbol: decode remainder of buffer when null-byte missing"
 (parameterize ([current-input-port (open-input-bytes (string->bytes/utf-8 "🍻"))])
   (check-equal? (decode (x:symbol #f 'utf8)) '🍻)))

(test-case
 "symbol: size should use symbol length"
 (check-equal? (send (x:symbol 7) x:size 'testing) 7))

(test-case
 "symbol: size should use correct encoding"
 (check-equal? (send (x:symbol 10 'utf8) x:size '🍻) 4))

(test-case
 "symbol: size should use encoding from function"
 (check-equal? (send (x:symbol 10 (λ _ 'utf8)) x:size '🍻) 4))

(test-case
 "symbol: should add size of length field before symbol"
 (check-equal? (send (x:symbol uint8 'utf8) x:size '🍻) 5))

; todo: it "should work with utf16be encoding"

(test-case
 "symbol: size should take null-byte into account"
 (check-equal? (send (x:symbol #f 'utf8) x:size '🍻) 5))

(test-case
 "symbol: size should use defined length if no value given"
 (check-equal? (send (x:symbol 10) x:size) 10))

(test-case
 "symbol: encode using symbol length"
 (parameterize ([current-output-port (open-output-bytes)])
   (encode (x:symbol 7) 'testing)
   (check-equal? (get-output-bytes (current-output-port)) #"testing")))

(test-case
 "symbol: encode using symbol length and pre-encode"
 (parameterize ([current-output-port (open-output-bytes)])
   (define xs (x:symbol 7 #:pre-encode (λ (val) (string->symbol (list->string (reverse (string->list (symbol->string val))))))))
   (encode xs 'testing)
   (check-equal? (get-output-bytes (current-output-port)) #"gnitset")))

(test-case
 "symbol: encode length as number before symbol"
 (parameterize ([current-output-port (open-output-bytes)])
   (encode (x:symbol uint8) 'testing)
   (check-equal? (get-output-bytes (current-output-port)) #"\x07testing")))

(test-case
 "symbol: encode length as number before symbol utf8"
 (parameterize ([current-output-port (open-output-bytes)])
   (encode (x:symbol uint8 'utf8) "testing 😜")
   (check-equal? (get-output-bytes (current-output-port)) (string->bytes/utf-8 "\x0ctesting 😜"))))

(test-case
 "symbol: encode utf8"
 (parameterize ([current-output-port (open-output-bytes)])
   (encode (x:symbol 4 'utf8) '🍻 )
   (check-equal? (get-output-bytes (current-output-port)) (string->bytes/utf-8 "🍻"))))

(test-case
 "symbol: encode encoding computed from function"
 (parameterize ([current-output-port (open-output-bytes)])
   (encode (x:symbol 4 (λ _ 'utf8)) '🍻)
   (check-equal? (get-output-bytes (current-output-port)) (string->bytes/utf-8 "🍻"))))

(test-case
 "symbol: encode null-terminated symbol"
 (parameterize ([current-output-port (open-output-bytes)])
   (encode (x:symbol #f 'utf8) '🍻  )
   (check-equal? (get-output-bytes (current-output-port)) (string->bytes/utf-8 "🍻\x00"))))