#lang racket/base
(require rackunit
         xenomorph
         racket/class)

(define Person
  (make-object Struct
    (list (cons 'name (make-object StringT uint8 'utf8))
          (cons 'age uint8))))

;; decode a person from a port
(define ip (open-input-bytes #"\4MikeA"))
(define x (send Person decode ip))

(module+ test
 (check-equal? (dict-ref x 'name) "Mike")
 (check-equal? (dict-ref x 'age) 65))

;; encode a person from a hash
(module+ test
 (check-equal? (send Person encode #f (hasheq 'name "Mike" 'age 65)) #"\4MikeA"))