#lang racket/base
(require rackunit
         racket/class
         "../number.rkt"
         "../enum.rkt"
         "../generic.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Enum.coffee
|#

(define e (+xenum #:type uint8
                  #:values '("foo" "bar" "baz")))

(test-case
 "should error with invalid type"
 (check-exn exn:fail:contract? (λ () (+xenum 42))))

(test-case
 "should error with invalid values"
 (check-exn exn:fail:contract? (λ () (+xenum #:values 42))))

(test-case
 "should have the right size"
 (check-equal? (size e) 1))

(test-case
 "decode should decode"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 0))])
   (check-equal? (decode e) "bar")
   (check-equal? (decode e) "baz")
   (check-equal? (decode e) "foo")))

(test-case
 "decode should decode with post-decode"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 0))])
   (define e2 (+xenum #:type uint8
                  #:values '("foo" "bar" "baz")
                  #:post-decode (λ (val) "foobar")))
   (check-equal? (decode e2) "foobar")
   (check-equal? (decode e2) "foobar")
   (check-equal? (decode e2) "foobar")))

(test-case
 "encode should encode"
 (parameterize ([current-output-port (open-output-bytes)])
   (encode e "bar")
   (encode e "baz")
   (encode e "foo")
   (check-equal? (get-output-bytes (current-output-port)) (bytes 1 2 0))))

(test-case
 "encode should encode with pre-encode"
 (parameterize ([current-output-port (open-output-bytes)])
   (define e2 (+xenum #:type uint8
                  #:values '("foo" "bar" "baz")
                  #:pre-encode (λ (val) "foo")))
   (encode e2 "bar")
   (encode e2 "baz")
   (encode e2 "foo")
   (check-equal? (get-output-bytes (current-output-port)) (bytes 0 0 0))))

(test-case
 "should throw on unknown option"
 (check-exn exn:fail:contract? (λ () (encode e "unknown" (open-output-bytes)))))