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
   (define array (+xlazy-array uint8 4))
   (define arr (decode array))
   (check-false (xarray? arr))
   (check-equal? (stream-length arr) 4)
   (check-equal? (pos (current-input-port)) 4)
   (check-equal? (stream-ref arr 0) 1)
   (check-equal? (stream-ref arr 1) 2)
   (check-equal? (stream-ref arr 2) 3)
   (check-equal? (stream-ref arr 3) 4)))

(test-case
 "should be able to convert to an array"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define array (+xlazy-array uint8 4))
   (define arr (decode array))
   (check-equal? (stream->list arr) '(1 2 3 4))))

(test-case
 "decode should decode length as number before array"
 (parameterize ([current-input-port (open-input-bytes (bytes 4 1 2 3 4 5))])
   (define array (+xlazy-array uint8 uint8))
   (define arr (decode array))
   (check-equal? (stream->list arr) '(1 2 3 4))))

(test-case
 "size should work with xlazy-arrays"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define array (+xlazy-array uint8 4))
   (define arr (decode array))
   (check-equal? (size array arr) 4)))

(test-case
 "encode should work with xlazy-arrays"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define array (+xlazy-array uint8 4))
   (define arr (decode array))  
   (check-equal? (encode array arr #f) (bytes 1 2 3 4))))
