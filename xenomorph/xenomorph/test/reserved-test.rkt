#lang racket/base
(require rackunit
         "../number.rkt"
         "../helper.rkt"
         "../reserved.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Reserved.coffee
|#

(test-case
 "size should have a default count of 1"
 (check-equal? (size (+xreserved uint8)) 1))

(test-case
 "size should allow custom counts and types"
 (check-equal? (size (+xreserved uint16be 10)) 20))

(test-case
 "should decode"
 (parameterize ([current-input-port (open-input-bytes (bytes 0 0))])
   (define reserved (+xreserved uint16be))
   (check-equal? (decode reserved) (void))
   (check-equal? (pos (current-input-port)) 2)))

(test-case
 "should decode with post-decode"
 (parameterize ([current-input-port (open-input-bytes (bytes 0 0))])
   (define reserved (+xreserved uint16be))
   (set-post-decode! reserved (λ (val) 42))
   (check-equal? (decode reserved) 42)
   (check-equal? (pos (current-input-port)) 2)))

(test-case
 "should encode"
 (parameterize ([current-output-port (open-output-bytes)])
   (define reserved (+xreserved uint16be))
   (encode reserved #f)
   (check-equal? (dump (current-output-port)) (bytes 0 0))))

(test-case
 "should encode with pre-encode"
 (parameterize ([current-output-port (open-output-bytes)])
   (define reserved (+xreserved uint32be))
   (set-pre-encode! reserved (λ (val) 42))
   (encode reserved #f)
   (check-equal? (dump (current-output-port)) (bytes 0 0 0 0))))