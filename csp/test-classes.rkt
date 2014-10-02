#lang racket
(require rackunit "main.rkt")

(check-equal? (get-field _solver (new Problem [solver 'solver-in])) 'solver-in)
(check-equal? (get-field _constraints (new Problem)) null)
(check-equal? (get-field _variables (new Problem)) (make-hash))

(define problem (new Problem)) ;; test from line 125
(send problem addVariable "a" '(1))
(check-equal? (get-field _list (hash-ref (get-field _variables problem) "a")) '(1)) 

(send problem reset)
(check-equal? (get-field _variables problem) (make-hash))
(send problem addVariables '("a" "b") '(1 2 3))
(check-equal? (get-field _list (hash-ref (get-field _variables problem) "a")) '(1 2 3))
(check-equal? (get-field _list (hash-ref (get-field _variables problem) "b")) '(1 2 3))


;; FunctionConstraint, two ways: implicit and explicit
(send problem reset)
(send problem addVariables '(a b) '(1 2))
(send problem addConstraint >) ; implicit
(check-hash-items (send problem getSolution) #hash((a . 1) (b . 2)))
(send problem reset)
(send problem addVariables '(a b) '(1 2))
(send problem addConstraint (new FunctionConstraint [func >])) ; explicit
(check-hash-items (send problem getSolution) #hash((a . 1) (b . 2)))

;; AllDifferentConstraint
(send problem reset)
(send problem addVariables '(a b) '(1 2))
(send problem addConstraint (new AllDifferentConstraint))
(let ([solutions (send problem getSolutions)])
  (check-equal? (hash-ref (first solutions) 'a) (hash-ref (second solutions) 'b))
  (check-equal? (hash-ref (second solutions) 'a) (hash-ref (first solutions) 'b)))


;; AllEqualConstraint
(send problem reset)
(send problem addVariables '(a b) '(1 2))
(send problem addConstraint (new AllEqualConstraint))
(let ([solutions (send problem getSolutions)])
  (check-equal? (hash-ref (first solutions) 'a) (hash-ref (first solutions) 'b))
  (check-equal? (hash-ref (second solutions) 'a) (hash-ref (second solutions) 'b)))


