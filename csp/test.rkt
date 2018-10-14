#lang at-exp racket
(require "csp.rkt" rackunit)

#|
(define creduce (assign-val ($csp (list ($var 'a '(1 2 3)) ($var 'b '(2 3)) ($var 'c '(1 2 3 4 5))) (list ($constraint '(a b c) (procedure-rename (λ (a b c) (= (+ a b c) 4)) 'summer)))) 'a 1))
(check-equal? 
 (make-arcs-consistent (reduce-constraint-arity creduce))
 ($csp (list ($var 'a '(1)) ($var 'b '(2)) ($var 'c '(1))) '()))

(define f (λ (a b c d) (+ a b c d)))
(check-equal? 10 ((reduce-arity f '(1 b c d)) 2 3 4))
(check-equal? 10 ((reduce-arity f '(1 2 c d)) 3 4))
(check-equal? 10 ((reduce-arity f '(1 2 3 d)) 4))
(check-equal? 10 ((reduce-arity f '(1 b 3 d)) 2 4))
(check-equal? 10 ((reduce-arity f '(a b 3 d)) 1 2 4))
|#

(make-nodes-consistent ($csp (list ($var 'a '(1)) ($var 'b '(0 1))) (list ($constraint '(b) zero?))))

(remove-assigned-constraints ($csp (list ($var 'a '(1)) ($var 'b '(0 1))) (list ($constraint '(b) zero?))))

