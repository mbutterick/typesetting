#lang racket/base
(require rackunit
         racket/class
         racket/vector
         "../vector.rkt"
         "../dict.rkt"
         "../number.rkt"
         "../pointer.rkt"
         "../base.rkt"
         sugar/unstable/dict)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Array.coffee
|#

(test-case 
 "vector: decode fixed length"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:vector #:type uint8 #:length 4)) '#(1 2 3 4))))

(test-case 
 "vector: decode nested"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:vector #:type (x:dict 'foo uint8) #:length 4))
                 (vector (mhasheq 'foo 1)
                       (mhasheq 'foo 2)
                       (mhasheq 'foo 3)
                       (mhasheq 'foo 4)))))

(test-case 
 "vector: decode with post-decode"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xa (x:vector #:type uint8 #:length 4 #:post-decode (λ (val) (vector-map (λ (x) (* 2 x)) val))))
   (check-equal? (decode xa) '#(2 4 6 8))))

(test-case 
 "vector: decode fixed number of bytes"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:vector #:type uint16be #:length 4 #:count-bytes #t)) '#(258 772))))

(test-case 
 "vector: decode length from parent key"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:vector #:type uint8 #:length (λ (p) (hash-ref p 'len))) (current-input-port) #:parent (mhash 'len 4)) '#(1 2 3 4))))

(test-case 
 "vector: decode byte count from parent key"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:vector #:type uint16be #:length (λ (p) (hash-ref p 'len)) #:count-bytes #t) (current-input-port) #:parent (mhash 'len 4)) '#(258 772))))

(test-case 
 "vector: decode length as number before array"
 (parameterize ([current-input-port (open-input-bytes (bytes 4 1 2 3 4 5))])
   (check-equal? (decode (x:vector #:type uint8 #:length uint8)) '#(1 2 3 4))))

(test-case 
 "vector: decode byte count as number before array"
 (parameterize ([current-input-port (open-input-bytes (bytes 4 1 2 3 4 5))])
   (check-equal? (decode (x:vector #:type uint16be #:length uint8 #:count-bytes #t)) '#(258 772))))

(test-case 
 "vector: decode length from function"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:vector #:type uint8 #:length (λ _ 4))) '#(1 2 3 4))))

(test-case 
 "vector: decode byte count from function"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:vector #:type uint16be #:length (λ _ 4) #:count-bytes #t)) '#(258 772))))

(test-case 
 "vector: decode to the end of parent if no length given"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:vector #:type uint8) (current-input-port) #:parent (mhash x:length-key 4 x:start-offset-key 0)) '#(1 2 3 4))))

(test-case 
 "vector: decode to the end of the stream if parent exists, but its length is 0"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:vector #:type uint8) (current-input-port) #:parent (mhash x:length-key 0 x:start-offset-key 0)) '#(1 2 3 4 5))))

(test-case 
 "vector: decode to the end of the stream if no parent and length is given"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4))])
   (check-equal? (decode (x:vector #:type uint8)) '#(1 2 3 4 ))))

(test-case 
 "vector: use array length"
 (check-equal? (size (x:vector #:type uint8 #:length 10) '#(1 2 3 4)) 4))

(test-case 
 "vector: add size of length field before string"
 (check-equal? (size (x:vector #:type uint8 #:length uint8) '#(1 2 3 4)) 5))

(test-case 
 "vector: use defined length if no value given"
 (check-equal? (size (x:vector #:type uint8 #:length 10)) 10))

(test-case 
 "vector: encode using array length"
 (check-equal? (encode (x:vector #:type uint8 #:length 4) '#(1 2 3 4) #f) (bytes 1 2 3 4)))

(test-case 
 "vector: encode with pre-encode"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xa (x:vector #:type uint8 #:length 4 #:pre-encode (λ (val) (vector-map (λ (x) (* 2 x)) val))))
   (check-equal? (encode xa '#(1 2 3 4) #f) (bytes 2 4 6 8))))

(test-case 
 "vector: encode length as number before array"
 (check-equal? (encode (x:vector #:type uint8 #:length uint8) '#(1 2 3 4) #f) (bytes 4 1 2 3 4)))

(test-case 
   "vector: add pointers after array if length is encoded at start"
   (check-equal? (encode (x:vector #:type (x:pointer uint8 #:dest-type uint8)
                                  #:length uint8) '#(1 2 3 4) #f) (bytes 4 5 6 7 8 1 2 3 4)))