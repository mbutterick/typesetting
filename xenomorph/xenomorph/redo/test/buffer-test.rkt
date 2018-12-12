#lang racket/base
(require rackunit
         sugar/unstable/dict
         "../buffer.rkt"
         "../number.rkt"
         "../helper.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Buffer.coffee
|#

(test-case
 "buffer should decode"
 (parameterize ([current-input-port (open-input-bytes (bytes #xab #xff #x1f #xb6))])
   (define buf (+xbuffer 2))
   (check-equal? (decode buf) (bytes #xab #xff))
   (check-equal? (decode buf) (bytes #x1f #xb6))))

(test-case
 "buffer should decode with parent key length"
 (parameterize ([current-input-port (open-input-bytes (bytes #xab #xff #x1f #xb6))])
   (define buf (+xbuffer 'len))
   (check-equal? (decode buf #:parent (hash 'len 3)) (bytes #xab #xff #x1f))
   (check-equal? (decode buf #:parent (hash 'len 1)) (bytes #xb6))))

(test-case
 "size should return size"
 (check-equal? (size (+xbuffer 2) (bytes #xab #xff)) 2))

(test-case
 "size should use defined length if no value given"
 (check-equal? (size (+xbuffer 10)) 10))

(test-case
 "encode should encode"
 (let ([buf (+xbuffer 2)])
   (check-equal? (bytes-append
                  (encode buf (bytes #xab #xff) #f)
                  (encode buf (bytes #x1f #xb6) #f)) (bytes #xab #xff #x1f #xb6))))

(test-case
 "encode should encode length before buffer"
 (check-equal? (encode (+xbuffer uint8) (bytes #xab #xff) #f) (bytes 2 #xab #xff)))