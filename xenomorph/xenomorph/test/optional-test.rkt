#lang racket/base
(require rackunit
         racket/class
         "../base.rkt"
         "../number.rkt"
         "../optional.rkt"
         "../generic.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Optional.coffee
|#

(test-case
 "optional: decode should not decode when condition is falsy"
 (parameterize ([current-input-port (open-input-bytes (bytes 0))])
   (define optional (x:optional #:type uint8 #:condition #f))
   (check-equal? (decode optional) (void))
   (check-equal? (pos (current-input-port)) 0)))

(test-case
 "optional: decode with post-decode"
 (parameterize ([current-input-port (open-input-bytes (bytes 0))])
   (define optional (x:optional #:type uint8 #:condition #f #:post-decode (λ (val) 42)))
   (check-equal? (decode optional) 42)
   (check-equal? (pos (current-input-port)) 0)))

(test-case
 "optional: decode should not decode when condition is a function and falsy"
 (parameterize ([current-input-port (open-input-bytes (bytes 0))])
   (define optional (x:optional #:type uint8 #:condition (λ _ #f)))
   (check-equal? (decode optional) (void))
   (check-equal? (pos (current-input-port)) 0)))

(test-case
 "optional: decode should decode when condition is omitted"
 (parameterize ([current-input-port (open-input-bytes (bytes 0))])
   (define optional (x:optional #:type uint8))
   (check-not-equal? (decode optional) (void))
   (check-equal? (pos (current-input-port)) 1)))

(test-case
 "optional: decode should decode when condition is truthy"
 (parameterize ([current-input-port (open-input-bytes (bytes 0))])
   (define optional (x:optional #:type uint8 #:condition #t))
   (check-not-equal? (decode optional) (void))
   (check-equal? (pos (current-input-port)) 1)))

(test-case
 "optional: decode should decode when condition is a function and truthy"
 (parameterize ([current-input-port (open-input-bytes (bytes 0))])
   (define optional (x:optional #:type uint8 #:condition (λ _ #t)))
   (check-not-equal? (decode optional) (void))
   (check-equal? (pos (current-input-port)) 1)))

(test-case
 "optional: size"
 (check-equal? (size (x:optional #:type uint8 #:condition #f)) 0))

(test-case
 "optional: size should return 0 when condition is a function and falsy"
 (check-equal? (size (x:optional #:type uint8 #:condition (λ _ #f))) 0))

(test-case
 "optional: size should return given type size when condition is omitted"
 (check-equal? (size (x:optional #:type uint8)) 1))

(test-case
 "optional: size should return given type size when condition is truthy"
 (check-equal? (size (x:optional #:type uint8 #:condition #t)) 1))

(test-case
 "optional: size should return given type size when condition is a function and truthy"
 (check-equal? (size (x:optional #:type uint8 #:condition (λ _ #t))) 1))

(test-case
 "optional: encode should not encode when condition is falsy"
 (parameterize ([current-output-port (open-output-bytes)])
   (define optional (x:optional #:type uint8 #:condition #f))
   (encode optional 128)
   (check-equal? (get-output-bytes (current-output-port)) (bytes))))

(test-case
 "optional: encode with pre-encode"
 (parameterize ([current-output-port (open-output-bytes)])
   (define optional (x:optional #:type uint8 #:pre-encode (λ (val) 42)))
   (encode optional 128)
   (check-equal? (get-output-bytes (current-output-port)) (bytes 42))))

(test-case
 "optional: encode should not encode when condition is a function and falsy"
 (parameterize ([current-output-port (open-output-bytes)])
   (define optional (x:optional #:type uint8 #:condition (λ _ #f)))
   (encode optional 128)
   (check-equal? (get-output-bytes (current-output-port)) (bytes))))

(test-case
 "optional: encode should encode when condition is omitted"
 (parameterize ([current-output-port (open-output-bytes)])
   (define optional (x:optional #:type uint8))
   (encode optional 128)
   (check-equal? (get-output-bytes (current-output-port)) (bytes 128))))

(test-case
 "optional: encode should encode when condition is truthy"
 (parameterize ([current-output-port (open-output-bytes)])
   (define optional (x:optional #:type uint8 #:condition #t))
   (encode optional 128)
   (check-equal? (get-output-bytes (current-output-port)) (bytes 128))))

(test-case
 "optional: encode should encode when condition is a function and truthy"
 (parameterize ([current-output-port (open-output-bytes)])
   (define optional (x:optional #:type uint8 #:condition (λ _ #t)))
   (encode optional 128)
   (check-equal? (get-output-bytes (current-output-port)) (bytes 128))))