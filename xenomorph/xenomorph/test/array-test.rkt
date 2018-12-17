#lang racket/base
(require rackunit
         racket/class
         "../array.rkt"
         "../number.rkt"
         "../pointer.rkt"
         "../generic.rkt"
         sugar/unstable/dict)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Array.coffee
|#

(test-case 
 "array: decode fixed length"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (+xarray #:type uint8 #:length 4)) '(1 2 3 4))))

(test-case 
 "array: decode with post-decode"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xa (+xarray #:type uint8 #:length 4 #:post-decode (λ (val) (map (λ (x) (* 2 x)) val))))
   (check-equal? (decode xa) '(2 4 6 8))))

(test-case 
 "array: decode fixed number of bytes"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (+xarray #:type uint16be #:length 4 #:count-bytes #t)) '(258 772))))

(test-case 
 "array: decode length from parent key"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (send (+xarray #:type uint8 #:length 'len) xxdecode (current-input-port) (mhash 'len 4)) '(1 2 3 4))))

(test-case 
 "array: decode byte count from parent key"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (send (+xarray #:type uint16be #:length 'len #:count-bytes #t) xxdecode (current-input-port) (mhash 'len 4)) '(258 772))))

(test-case 
 "array: decode length as number before array"
 (parameterize ([current-input-port (open-input-bytes (bytes 4 1 2 3 4 5))])
   (check-equal? (decode (+xarray #:type uint8 #:length uint8)) '(1 2 3 4))))

(test-case 
 "array: decode byte count as number before array"
 (parameterize ([current-input-port (open-input-bytes (bytes 4 1 2 3 4 5))])
   (check-equal? (decode (+xarray #:type uint16be #:length uint8 #:count-bytes #t)) '(258 772))))

(test-case 
 "array: decode length from function"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (+xarray #:type uint8 #:length (λ _ 4))) '(1 2 3 4))))

(test-case 
 "array: decode byte count from function"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (+xarray #:type uint16be #:length (λ _ 4) #:count-bytes #t)) '(258 772))))

(test-case 
 "array: decode to the end of parent if no length given"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (send (+xarray #:type uint8) xxdecode (current-input-port) (mhash '_length 4 '_startOffset 0)) '(1 2 3 4))))

(test-case 
 "array: decode to the end of the stream if parent exists, but its length is 0"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (send (+xarray #:type uint8) xxdecode (current-input-port) (mhash '_length 0 '_startOffset 0)) '(1 2 3 4 5))))

(test-case 
 "array: decode to the end of the stream if no parent and length is given"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4))])
   (check-equal? (decode (+xarray #:type uint8)) '(1 2 3 4 ))))

(test-case 
 "array: use array length"
 (check-equal? (size (+xarray #:type uint8 #:length 10) '(1 2 3 4)) 4))

(test-case 
 "array: add size of length field before string"
 (check-equal? (size (+xarray #:type uint8 #:length uint8) '(1 2 3 4)) 5))

(test-case 
 "array: use defined length if no value given"
 (check-equal? (size (+xarray #:type uint8 #:length 10)) 10))

(test-case 
 "array: encode using array length"
 (check-equal? (encode (+xarray #:type uint8 #:length 10) '(1 2 3 4) #f) (bytes 1 2 3 4)))

(test-case 
 "array: encode with pre-encode"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xa (+xarray #:type uint8 #:length 4 #:pre-encode (λ (val) (map (λ (x) (* 2 x)) val))))
   (check-equal? (encode xa '(1 2 3 4) #f) (bytes 2 4 6 8))))

(test-case 
 "array: encode length as number before array"
 (check-equal? (encode (+xarray #:type uint8 #:length uint8) '(1 2 3 4) #f) (bytes 4 1 2 3 4)))

(test-case 
   "array: add pointers after array if length is encoded at start"
   (check-equal? (encode (+xarray #:type (+xpointer #:offset-type uint8
                                             #:type uint8)
                                  #:length uint8) '(1 2 3 4) #f) (bytes 4 5 6 7 8 1 2 3 4)))