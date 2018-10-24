#lang csp
(require csp racket/list)

#:output foo

(define-variable q (range 33))

foo

(define-variable n (range 33))

(define-constraint c (λ (q n) (= (+ q n) 33)) '(q n))

(solve foo)