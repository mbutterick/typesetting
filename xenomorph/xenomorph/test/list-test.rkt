#lang racket/base
(require rackunit
         racket/class
         "../list.rkt"
         "../struct.rkt"
         "../number.rkt"
         "../pointer.rkt"
         "../base.rkt"
         sugar/unstable/dict)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Array.coffee
|#

(test-case 
 "list: decode fixed length"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:list #:type uint8 #:length 4)) '(1 2 3 4))))

(test-case 
 "list: decode nested"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:list #:type (x:struct 'foo uint8) #:length 4))
                 (list (mhasheq 'foo 1)
                       (mhasheq 'foo 2)
                       (mhasheq 'foo 3)
                       (mhasheq 'foo 4)))))

(test-case 
 "list: decode with post-decode"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xa (x:list #:type uint8 #:length 4 #:post-decode (λ (val) (map (λ (x) (* 2 x)) val))))
   (check-equal? (decode xa) '(2 4 6 8))))

(test-case 
 "list: decode fixed number of bytes"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:list #:type uint16be #:length 4 #:count-bytes #t)) '(258 772))))

(test-case 
 "list: decode length from parent key"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:list #:type uint8 #:length 'len) (current-input-port) #:parent (mhash 'len 4)) '(1 2 3 4))))

(test-case 
 "list: decode byte count from parent key"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:list #:type uint16be #:length 'len #:count-bytes #t) (current-input-port) #:parent (mhash 'len 4)) '(258 772))))

(test-case 
 "list: decode length as number before array"
 (parameterize ([current-input-port (open-input-bytes (bytes 4 1 2 3 4 5))])
   (check-equal? (decode (x:list #:type uint8 #:length uint8)) '(1 2 3 4))))

(test-case 
 "list: decode byte count as number before array"
 (parameterize ([current-input-port (open-input-bytes (bytes 4 1 2 3 4 5))])
   (check-equal? (decode (x:list #:type uint16be #:length uint8 #:count-bytes #t)) '(258 772))))

(test-case 
 "list: decode length from function"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:list #:type uint8 #:length (λ _ 4))) '(1 2 3 4))))

(test-case 
 "list: decode byte count from function"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:list #:type uint16be #:length (λ _ 4) #:count-bytes #t)) '(258 772))))

(test-case 
 "list: decode to the end of parent if no length given"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:list #:type uint8) (current-input-port) #:parent (mhash x:length-key 4 x:start-offset-key 0)) '(1 2 3 4))))

(test-case 
 "list: decode to the end of the stream if parent exists, but its length is 0"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (x:list #:type uint8) (current-input-port) #:parent (mhash x:length-key 0 x:start-offset-key 0)) '(1 2 3 4 5))))

(test-case 
 "list: decode to the end of the stream if no parent and length is given"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4))])
   (check-equal? (decode (x:list #:type uint8)) '(1 2 3 4 ))))

(test-case 
 "list: use array length"
 (check-equal? (size (x:list #:type uint8 #:length 10) '(1 2 3 4)) 4))

(test-case 
 "list: add size of length field before string"
 (check-equal? (size (x:list #:type uint8 #:length uint8) '(1 2 3 4)) 5))

(test-case 
 "list: use defined length if no value given"
 (check-equal? (size (x:list #:type uint8 #:length 10)) 10))

(test-case 
 "list: encode using array length"
 (check-equal? (encode (x:list #:type uint8 #:length 4) '(1 2 3 4) #f) (bytes 1 2 3 4)))

(test-case 
 "list: encode with pre-encode"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xa (x:list #:type uint8 #:length 4 #:pre-encode (λ (val) (map (λ (x) (* 2 x)) val))))
   (check-equal? (encode xa '(1 2 3 4) #f) (bytes 2 4 6 8))))

(test-case 
 "list: encode length as number before array"
 (check-equal? (encode (x:list #:type uint8 #:length uint8) '(1 2 3 4) #f) (bytes 4 1 2 3 4)))

(test-case 
   "list: add pointers after array if length is encoded at start"
   (check-equal? (encode (x:list #:type (x:pointer #:offset-type uint8
                                             #:type uint8)
                                  #:length uint8) '(1 2 3 4) #f) (bytes 4 5 6 7 8 1 2 3 4)))