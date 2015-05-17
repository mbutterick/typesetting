#lang racket
(require rackunit "main.rkt")


;; Problem: fields
(check-equal? (get-field _solver (new problem% [solver 'solver-in])) 'solver-in)
(check-equal? (get-field _constraints (new problem%)) null)
(check-equal? (get-field _variable-domains (new problem%)) (make-hash))

(define problem null)

;; Problem: reset
(set! problem (new problem%))
(define early-solutions (send problem get-solutions))
(send problem add-variable "a" (range 3))
(check-not-equal? (send problem get-solutions) early-solutions)
(send problem reset)
(check-equal? (send problem get-solutions) early-solutions)

;; Problem: setSolver & get-solver
(define solver (new backtracking-solver%))
(set! problem (new problem% [solver solver]))
(check-true (solver%? (send problem get-solver)))

;; Problem: add-variable
(set! problem (new problem%))
(send problem add-variable "a" '(1 2))
(check-true (or (= (hash-ref (send problem get-solution) "a") 1)
                (= (hash-ref (send problem get-solution) "a") 2)))
(check-exn exn:fail? (λ () (send problem add-variable "b" null))) ;; empty domain


;; Problem: add-variables
(set! problem (new problem%))
(send problem add-variables '("a" "b") '(1 2 3))
(check-equal? (length (send problem get-solutions)) 9)

;; Problem: add-constraint
(set! problem (new problem%))
(send problem add-variables '("a" "b") '(1 2 3))
(send problem add-constraint (λ(a b) (= a (add1 b))))
(check-equal? (length (send problem get-solutions)) 2)


;; FunctionConstraint, two ways: implicit and explicit
(send problem reset)
(send problem add-variables '(a b) '(1 2))
(send problem add-constraint <) ; implicit
(check-hash-items (send problem get-solution) #hash((a . 1) (b . 2)))
(send problem reset)
(send problem add-variables '(a b) '(1 2))
(send problem add-constraint (new function-constraint% [func <])) ; explicit
(check-hash-items (send problem get-solution) #hash((a . 1) (b . 2)))

;; AllDifferentConstraint
(send problem reset)
(send problem add-variables '(a b) '(1 2))
(send problem add-constraint (new all-different-constraint%))
(let ([solutions (send problem get-solutions)])
  (check-equal? (hash-ref (first solutions) 'a) (hash-ref (second solutions) 'b))
  (check-equal? (hash-ref (second solutions) 'a) (hash-ref (first solutions) 'b)))


;; AllEqualConstraint
(send problem reset)
(send problem add-variables '(a b) '(1 2))
(send problem add-constraint (new all-equal-constraint%))
(let ([solutions (send problem get-solutions)])
  (check-equal? (hash-ref (first solutions) 'a) (hash-ref (first solutions) 'b))
  (check-equal? (hash-ref (second solutions) 'a) (hash-ref (second solutions) 'b)))


