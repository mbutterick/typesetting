#lang restructure/racket
(require "main.rkt")

(define Person
  (make-object RStruct
    (list (cons 'name (make-object RString uint8 'utf8))
          (cons 'age uint8))))

;; decode a person from a buffer
(define stream-in (make-object RDecodeStream #"\4MikeA"))
(define x (send Person decode stream-in))

(test-module
 (check-equal? (hash-ref x 'name) "Mike")
 (check-equal? (hash-ref x 'age) 65))

;; encode a person from a hash
(define stream-out (make-object REncodeStream))
(send Person encode stream-out (hasheq 'name "Mike" 'age 65))

(test-module
 (check-equal? (send stream-out dump) #"\4MikeA"))