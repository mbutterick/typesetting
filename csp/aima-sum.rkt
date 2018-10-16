#lang br
(require "aima.rkt")
(define vs '(a b c))

(define ds (for/hash ([k vs])
             (values k (range 10))))
(define ns (for*/hash ([v (in-list vs)])
             (values v (remove v vs))))
(define csp (make-csp vs ds ns (λ (A a B b) (not (eq? a b)))))
(solve csp)
(nassigns csp)
(nchecks csp)