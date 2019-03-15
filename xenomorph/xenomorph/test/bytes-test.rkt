#lang racket/base
(require rackunit
         racket/class
         "../bytes.rkt"
         "../number.rkt"
         "../base.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Buffer.coffee
|#

(test-case
 "bytes: should decode"
 (parameterize ([current-input-port (open-input-bytes (bytes #xab #xff #x1f #xb6))])
   (define buf (x:bytes #:length 2))
   (check-equal? (decode buf) (bytes #xab #xff))
   (check-equal? (decode buf) (bytes #x1f #xb6))))

(test-case
 "bytes: should error on invalid length"
 (check-exn exn:fail:contract? (λ () (x:bytes #:length #true))))

(test-case
 "bytes: should decode with post-decode"
 (parameterize ([current-input-port (open-input-bytes (bytes #xab #xff #x1f #xb6))])
   (define buf (x:bytes #:length 2 #:post-decode (λ (val) (bytes 1 2))))
   (check-equal? (decode buf) (bytes 1 2))
   (check-equal? (decode buf) (bytes 1 2))))

(test-case
 "bytes: should decode with parent key length"
 (parameterize ([current-input-port (open-input-bytes (bytes #xab #xff #x1f #xb6))])
   (define buf (x:bytes #:length 'len))
   (check-equal? (decode buf #:parent (hash 'len 3)) (bytes #xab #xff #x1f))
   (check-equal? (decode buf #:parent (hash 'len 1)) (bytes #xb6))))

(test-case
 "bytes: hould return size"
 (check-equal? (size (x:bytes #:length 2) (bytes #xab #xff)) 2))

(test-case
 "bytes: hould use defined length if no value given"
 (check-equal? (size (x:bytes #:length 10)) 10))

(test-case
 "bytes: should encode"
 (let ([buf (x:bytes 2)])
   (check-equal? (bytes-append
                  (encode buf (bytes #xab #xff) #f)
                  (encode buf (bytes #x1f #xb6) #f)) (bytes #xab #xff #x1f #xb6))))

(test-case
 "bytes: should encode with pre-encode"
 (let ()
   (define buf (x:bytes 2 #:pre-encode (λ (val) (bytes 1 2))))
   (check-equal? (bytes-append
                  (encode buf (bytes #xab #xff) #f)
                  (encode buf (bytes #x1f #xb6) #f)) (bytes 1 2 1 2))))

(test-case
 "bytes: should encode length before bytes"
 (check-equal? (encode (x:bytes #:length uint8) (bytes #xab #xff) #f) (bytes 2 #xab #xff)))