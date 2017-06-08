#lang restructure/racket
(require "main.rkt")

(define Person
  (make-object RStruct
    (mhash 'name uint16
           'age uint8)))

;; decode a person from a buffer
(define stream (open-input-bytes #"ABC"))
(define x (send Person decode stream))

(test-module
 (check-equal? (hash-ref x 'name) 16961)
 (check-equal? (hash-ref x 'age) 67))

;; encode a person from a hash
(define out (open-output-bytes))
(send Person encode out (hasheq 'name 16961 'age 67))

(test-module
 (check-equal? (get-output-bytes out) #"ABC"))