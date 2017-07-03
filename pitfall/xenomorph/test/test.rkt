#lang reader (submod "racket.rkt" reader)

(define Person
  (make-object Struct
    (list (cons 'name (make-object StringT uint8 'utf8))
          (cons 'age uint8))))

;; decode a person from a buffer
(define stream-in (make-object DecodeStream #"\4MikeA"))
(define x (send Person decode stream-in))

(test-module
 (check-equal? (dict-ref x 'name) "Mike")
 (check-equal? (dict-ref x 'age) 65))

;; encode a person from a hash
(test-module
 (check-equal? (send Person encode #f (hasheq 'name "Mike" 'age 65)) #"\4MikeA"))