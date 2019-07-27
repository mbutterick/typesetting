#lang racket/base
(require rackunit
         racket/class
         racket/stream
         "../list.rkt"
         "../base.rkt"
         "../number.rkt"
         "../stream.rkt"
         "../base.rkt")

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/LazyArray.coffee
|#

(test-case
 "stream: decode should decode items lazily"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xla (x:stream uint8 4))
   (define arr (decode xla))
   (check-equal? (stream-length arr) 4)
   (check-equal? (pos (current-input-port)) 4)
   (check-equal? (stream-ref arr 0) 1)
   (check-equal? (stream-ref arr 1) 2)
   (check-equal? (stream-ref arr 2) 3)
   (check-equal? (stream-ref arr 3) 4)))

(test-case
 "stream: decode should decode items lazily with post-decode"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xla (x:stream uint8 4 #:post-decode (λ (str) (stream-map (λ (i) (* 2 i)) str))))
   (define arr (decode xla))
   (check-false (x:list? arr))
   (check-equal? (stream-length arr) 4)
   (check-equal? (pos (current-input-port)) 4)
   (check-equal? (stream-ref arr 0) 2)
   (check-equal? (stream-ref arr 1) 4)
   (check-equal? (stream-ref arr 2) 6)
   (check-equal? (stream-ref arr 3) 8)))

(test-case
 "stream: should be able to convert to an array"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xla (x:stream uint8 4))
   (define arr (decode xla))
   (check-equal? (stream->list arr) '(1 2 3 4))))

(test-case
 "stream: decode should decode length as number before array"
 (parameterize ([current-input-port (open-input-bytes (bytes 4 1 2 3 4 5))])
   (define xla (x:stream uint8 uint8))
   (define arr (decode xla))
   (check-equal? (stream->list arr) '(1 2 3 4))))

(test-case
 "stream: size should work with xlazy-arrays"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xla (x:stream uint8 4))
   (define arr (decode xla))
   (check-equal? (send xla x:size arr) 4)))

(test-case
 "stream: encode should work with xlazy-arrays"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xla (x:stream uint8 4))
   (define arr (decode xla))  
   (check-equal? (encode xla arr #f) (bytes 1 2 3 4))))

(test-case
 "stream: encode should work with xlazy-arrays with pre-encode"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xla (x:stream uint8 4 #:pre-encode (λ (str) (stream-map (λ (val) (* 2 val)) str))))
   (define arr (decode xla))  
   (check-equal? (encode xla arr #f) (bytes 2 4 6 8))))
