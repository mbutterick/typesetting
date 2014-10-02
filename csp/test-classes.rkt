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