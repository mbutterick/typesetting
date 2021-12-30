#lang racket/base
(require rackunit
         racket/class
         "../number.rkt"
         "../base.rkt"
         "../reserved.rkt"
         "../base.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Reserved.coffee
|#

(test-case
 "reserved: size should have a default count of 1"
 (check-equal? (send (x:reserved uint8) x:size) 1))

(test-case
 "reserved: size should allow custom counts and types"
 (check-equal? (send (x:reserved uint16be 10) x:size) 20))

(test-case
 "reserved: should decode"
 (parameterize ([current-input-port (open-input-bytes (bytes 0 0))])
   (define reserved (x:reserved uint16be))
   (check-equal? (decode reserved) (void))
   (check-equal? (pos (current-input-port)) 2)))

(test-case
 "reserved: should decode with post-decode"
 (parameterize ([current-input-port (open-input-bytes (bytes 0 0))])
   (define reserved (x:reserved uint16be #:post-decode (λ (val) 42)))
   (check-equal? (decode reserved) 42)
   (check-equal? (pos (current-input-port)) 2)))

(test-case
 "reserved: should encode"
 (parameterize ([current-output-port (open-output-bytes)])
   (define reserved (x:reserved uint16be))
   (encode reserved #f)
   (check-equal? (get-output-bytes (current-output-port)) (bytes 0 0))))

(test-case
 "reserved: should encode with pre-encode"
 (parameterize ([current-output-port (open-output-bytes)])
   (define reserved (x:reserved uint32be #:pre-encode (λ (val) 42)))
   (encode reserved #f)
   (check-equal? (get-output-bytes (current-output-port)) (bytes 0 0 0 0))))