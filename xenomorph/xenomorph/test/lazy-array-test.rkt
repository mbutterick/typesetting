#lang racket/base
(require rackunit
         racket/dict
         racket/stream
         "../array.rkt"
         "../helper.rkt"
         "../number.rkt"
         "../lazy-array.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/LazyArray.coffee
|#

(test-case
 "decode should decode items lazily"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xla (+xlazy-array uint8 4))
   (define arr (decode xla))
   (check-false (xarray? arr))
   (check-equal? (stream-length arr) 4)
   (check-equal? (pos (current-input-port)) 4)
   (check-equal? (stream-ref arr 0) 1)
   (check-equal? (stream-ref arr 1) 2)
   (check-equal? (stream-ref arr 2) 3)
   (check-equal? (stream-ref arr 3) 4)))

(test-case
 "decode should decode items lazily with post-decode"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xla (+xlazy-array uint8 4))
   (set-post-decode! xla (λ (val) (* 2 val)))
   (define arr (decode xla))
   (check-false (xarray? arr))
   (check-equal? (stream-length arr) 4)
   (check-equal? (pos (current-input-port)) 4)
   (check-equal? (stream-ref arr 0) 2)
   (check-equal? (stream-ref arr 1) 4)
   (check-equal? (stream-ref arr 2) 6)
   (check-equal? (stream-ref arr 3) 8)))

(test-case
 "should be able to convert to an array"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xla (+xlazy-array uint8 4))
   (define arr (decode xla))
   (check-equal? (stream->list arr) '(1 2 3 4))))

(test-case
 "decode should decode length as number before array"
 (parameterize ([current-input-port (open-input-bytes (bytes 4 1 2 3 4 5))])
   (define xla (+xlazy-array uint8 uint8))
   (define arr (decode xla))
   (check-equal? (stream->list arr) '(1 2 3 4))))

(test-case
 "size should work with xlazy-arrays"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xla (+xlazy-array uint8 4))
   (define arr (decode xla))
   (check-equal? (size xla arr) 4)))

(test-case
 "encode should work with xlazy-arrays"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xla (+xlazy-array uint8 4))
   (define arr (decode xla))  
   (check-equal? (encode xla arr #f) (bytes 1 2 3 4))))

(test-case
 "encode should work with xlazy-arrays with pre-encode"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xla (+xlazy-array uint8 4))
   (set-pre-encode! xla (λ (vals) (map (λ (val) (* 2 val)) vals)))
   (define arr (decode xla))  
   (check-equal? (encode xla arr #f) (bytes 2 4 6 8))))
