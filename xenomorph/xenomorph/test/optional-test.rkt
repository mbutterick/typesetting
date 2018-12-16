#lang racket/base
(require rackunit
         racket/class
         "../helper.rkt"
         "../number.rkt"
         "../optional.rkt"
         "../generic.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Optional.coffee
|#

(test-case
 "decode should not decode when condition is falsy"
 (parameterize ([current-input-port (open-input-bytes (bytes 0))])
   (define optional (+xoptional #:type uint8 #:condition #f))
   (check-equal? (decode optional) (void))
   (check-equal? (pos (current-input-port)) 0)))

(test-case
 "decode with post-decode"
 (parameterize ([current-input-port (open-input-bytes (bytes 0))])
   (define myxopt% (class xoptional%
                         (super-new)
                         (define/override (post-decode val) 42)))
   (define optional (+xoptional #:type uint8 #:condition #f #:subclass myxopt%))
   (check-equal? (decode optional) 42)
   (check-equal? (pos (current-input-port)) 0)))

(test-case
 "decode should not decode when condition is a function and falsy"
 (parameterize ([current-input-port (open-input-bytes (bytes 0))])
   (define optional (+xoptional #:type uint8 #:condition (λ _ #f)))
   (check-equal? (decode optional) (void))
   (check-equal? (pos (current-input-port)) 0)))

(test-case
 "decode should decode when condition is omitted"
 (parameterize ([current-input-port (open-input-bytes (bytes 0))])
   (define optional (+xoptional #:type uint8))
   (check-not-equal? (decode optional) (void))
   (check-equal? (pos (current-input-port)) 1)))

(test-case
 "decode should decode when condition is truthy"
 (parameterize ([current-input-port (open-input-bytes (bytes 0))])
   (define optional (+xoptional #:type uint8 #:condition #t))
   (check-not-equal? (decode optional) (void))
   (check-equal? (pos (current-input-port)) 1)))

(test-case
 "decode should decode when condition is a function and truthy"
 (parameterize ([current-input-port (open-input-bytes (bytes 0))])
   (define optional (+xoptional #:type uint8 #:condition (λ _ #t)))
   (check-not-equal? (decode optional) (void))
   (check-equal? (pos (current-input-port)) 1)))

(test-case
 "size"
 (check-equal? (size (+xoptional #:type uint8 #:condition #f)) 0))

(test-case
 "size should return 0 when condition is a function and falsy"
 (check-equal? (size (+xoptional #:type uint8 #:condition (λ _ #f))) 0))

(test-case
 "size should return given type size when condition is omitted"
 (check-equal? (size (+xoptional #:type uint8)) 1))

(test-case
 "size should return given type size when condition is truthy"
 (check-equal? (size (+xoptional #:type uint8 #:condition #t)) 1))

(test-case
 "size should return given type size when condition is a function and truthy"
 (check-equal? (size (+xoptional #:type uint8 #:condition (λ _ #t))) 1))

(test-case
 "encode should not encode when condition is falsy"
 (parameterize ([current-output-port (open-output-bytes)])
   (define optional (+xoptional #:type uint8 #:condition #f))
   (encode optional 128)
   (check-equal? (get-output-bytes (current-output-port)) (bytes))))

(test-case
 "encode with pre-encode"
 (parameterize ([current-output-port (open-output-bytes)])
   (define myxopt% (class xoptional%
                         (super-new)
                         (define/override (pre-encode val) 42)))
   (define optional (+xoptional #:type uint8 #:subclass myxopt%))
   (encode optional 128)
   (check-equal? (get-output-bytes (current-output-port)) (bytes 42))))

(test-case
 "encode should not encode when condition is a function and falsy"
 (parameterize ([current-output-port (open-output-bytes)])
   (define optional (+xoptional #:type uint8 #:condition (λ _ #f)))
   (encode optional 128)
   (check-equal? (get-output-bytes (current-output-port)) (bytes))))

(test-case
 "encode should encode when condition is omitted"
 (parameterize ([current-output-port (open-output-bytes)])
   (define optional (+xoptional #:type uint8))
   (encode optional 128)
   (check-equal? (get-output-bytes (current-output-port)) (bytes 128))))

(test-case
 "encode should encode when condition is truthy"
 (parameterize ([current-output-port (open-output-bytes)])
   (define optional (+xoptional #:type uint8 #:condition #t))
   (encode optional 128)
   (check-equal? (get-output-bytes (current-output-port)) (bytes 128))))

(test-case
 "encode should encode when condition is a function and truthy"
 (parameterize ([current-output-port (open-output-bytes)])
   (define optional (+xoptional #:type uint8 #:condition (λ _ #t)))
   (encode optional 128)
   (check-equal? (get-output-bytes (current-output-port)) (bytes 128))))