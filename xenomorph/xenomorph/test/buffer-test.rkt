#lang racket/base
(require rackunit
         racket/class
         "../buffer.rkt"
         "../number.rkt"
         "../generic.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Buffer.coffee
|#

(test-case
 "buffer: should decode"
 (parameterize ([current-input-port (open-input-bytes (bytes #xab #xff #x1f #xb6))])
   (define buf (+xbuffer #:length 2))
   (check-equal? (decode buf) (bytes #xab #xff))
   (check-equal? (decode buf) (bytes #x1f #xb6))))

(test-case
 "buffer: should error on invalid length"
 (check-exn exn:fail:contract? (λ () (+xbuffer #:length #true))))

(test-case
 "buffer: should decode with post-decode"
 (parameterize ([current-input-port (open-input-bytes (bytes #xab #xff #x1f #xb6))])
   (define buf (+xbuffer #:length 2 #:post-decode (λ (val) (bytes 1 2))))
   (check-equal? (decode buf) (bytes 1 2))
   (check-equal? (decode buf) (bytes 1 2))))

(test-case
 "buffer: should decode with parent key length"
 (parameterize ([current-input-port (open-input-bytes (bytes #xab #xff #x1f #xb6))])
   (define buf (+xbuffer #:length 'len))
   (check-equal? (decode buf #:parent (hash 'len 3)) (bytes #xab #xff #x1f))
   (check-equal? (decode buf #:parent (hash 'len 1)) (bytes #xb6))))

(test-case
 "buffer: hould return size"
 (check-equal? (size (+xbuffer #:length 2) (bytes #xab #xff)) 2))

(test-case
 "buffer: hould use defined length if no value given"
 (check-equal? (size (+xbuffer #:length 10)) 10))

(test-case
 "buffer: should encode"
 (let ([buf (+xbuffer 2)])
   (check-equal? (bytes-append
                  (encode buf (bytes #xab #xff) #f)
                  (encode buf (bytes #x1f #xb6) #f)) (bytes #xab #xff #x1f #xb6))))

(test-case
 "buffer: should encode with pre-encode"
 (let ()
   (define buf (+xbuffer 2 #:pre-encode (λ (val) (bytes 1 2))))
   (check-equal? (bytes-append
                  (encode buf (bytes #xab #xff) #f)
                  (encode buf (bytes #x1f #xb6) #f)) (bytes 1 2 1 2))))

(test-case
 "buffer: should encode length before buffer"
 (check-equal? (encode (+xbuffer #:length uint8) (bytes #xab #xff) #f) (bytes 2 #xab #xff)))