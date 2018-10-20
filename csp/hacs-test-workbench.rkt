#lang debug racket
(require sugar "hacs.rkt")

(current-inference forward-check)
(current-select-variable mrv)
(current-order-values shuffle)
(current-shuffle #true)

(define xsum (make-csp))
(add-vars! xsum '(l1 l2 l3 l4 r1 r2 r3 r4 x) (range 1 10))
(add-pairwise-constraint! xsum < '(l1 l2 l3 l4))
(add-pairwise-constraint! xsum < '(r1 r2 r3 r4))
(add-constraint! xsum (λ (l1 l2 l3 l4 x) (= 27 (+ l1 l2 l3 l4 x))) '(l1 l2 l3 l4 x))
(add-constraint! xsum (λ (r1 r2 r3 r4 x)  (= 27 (+ r1 r2 r3 r4 x))) '(r1 r2 r3 r4 x))
(add-pairwise-constraint! xsum alldiff= '(l1 l2 l3 l4 r1 r2 r3 r4 x))

(time (solve xsum))
