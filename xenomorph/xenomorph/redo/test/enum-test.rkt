#lang racket/base
(require rackunit
         sugar/unstable/dict
         "../helper.rkt"
         "../number.rkt"
         "../enum.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Enum.coffee
|#

(define e (+xenum uint8 '("foo" "bar" "baz")))

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
 "encode should encode"
 (parameterize ([current-output-port (open-output-bytes)])
   (encode e "bar")
   (encode e "baz")
   (encode e "foo")
   (check-equal? (dump (current-output-port)) (bytes 1 2 0))))

(test-case
 "should throw on unknown option"
 (check-exn exn:fail:contract? (λ () (encode e "unknown" (open-output-bytes)))))