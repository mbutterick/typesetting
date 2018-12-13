#lang racket/base
(require rackunit
         "../helper.rkt"
         "../array.rkt"
         "../number.rkt"
         "../pointer.rkt"
         sugar/unstable/dict)

#|
approximates
https://github.com/mbutterick/restructure/blob/master/test/Array.coffee
|#

(test-case 
 "decode fixed length"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (+xarray uint8 4)) '(1 2 3 4))))

(test-case 
 "decode with post-decode"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xa (+xarray uint8 4))
   (set-post-decode! xa (λ (val . _) (map (λ (x) (* 2 x)) val)))
   (check-equal? (decode xa) '(2 4 6 8))))

(test-case 
 "decode fixed number of bytes"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (+xarray uint16be 4 'bytes)) '(258 772))))

(test-case 
 "decode length from parent key"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (+xarray uint8 'len) #:parent (mhash 'len 4)) '(1 2 3 4))))

(test-case 
 "decode byte count from parent key"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (+xarray uint16be 'len 'bytes) #:parent (mhash 'len 4)) '(258 772))))

(test-case 
 "decode length as number before array"
 (parameterize ([current-input-port (open-input-bytes (bytes 4 1 2 3 4 5))])
   (check-equal? (decode (+xarray uint8 uint8)) '(1 2 3 4))))

(test-case 
 "decode byte count as number before array"
 (parameterize ([current-input-port (open-input-bytes (bytes 4 1 2 3 4 5))])
   (check-equal? (decode (+xarray uint16be uint8 'bytes)) '(258 772))))

(test-case 
 "decode length from function"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (+xarray uint8 (λ _ 4))) '(1 2 3 4))))

(test-case 
 "decode byte count from function"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (+xarray uint16be (λ _ 4) 'bytes)) '(258 772))))

(test-case 
 "decode to the end of parent if no length given"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (+xarray uint8) #:parent (mhash '_length 4 '_startOffset 0)) '(1 2 3 4))))

(test-case 
 "decode to the end of the stream if parent exists, but its length is 0"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (check-equal? (decode (+xarray uint8) #:parent (mhash '_length 0 '_startOffset 0)) '(1 2 3 4 5))))

(test-case 
 "decode to the end of the stream if no parent and length is given"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4))])
   (check-equal? (decode (+xarray uint8)) '(1 2 3 4 ))))

(test-case 
 "use array length"
 (check-equal? (size (+xarray uint8 10) '(1 2 3 4)) 4))

(test-case 
 "add size of length field before string"
 (check-equal? (size (+xarray uint8 uint8) '(1 2 3 4)) 5))

(test-case 
 "use defined length if no value given"
 (check-equal? (size (+xarray uint8 10)) 10))

(test-case 
 "encode using array length"
 (check-equal? (encode (+xarray uint8 10) '(1 2 3 4) #f) (bytes 1 2 3 4)))

(test-case 
 "encode with pre-encode"
 (parameterize ([current-input-port (open-input-bytes (bytes 1 2 3 4 5))])
   (define xa (+xarray uint8 4))
   (set-pre-encode! xa (λ (val . _) (map (λ (x) (* 2 x)) val)))
   (check-equal? (encode xa '(1 2 3 4) #f) (bytes 2 4 6 8))))

(test-case 
 "encode length as number before array"
 (check-equal? (encode (+xarray uint8 uint8) '(1 2 3 4) #f) (bytes 4 1 2 3 4)))

(test-case 
   "add pointers after array if length is encoded at start"
   (check-equal? (encode (+xarray (+xpointer #:offset-type uint8
                                             #:type uint8) uint8) '(1 2 3 4) #f) (bytes 4 5 6 7 8 1 2 3 4)))