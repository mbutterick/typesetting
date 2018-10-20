#lang debug racket
(require "hacs.rkt" rackunit)

(current-inference forward-check)
(current-select-variable mrv-degree-hybrid)
(current-order-values shuffle)
(current-shuffle #true)

(check-equal? (first-unassigned-variable ($csp (list ($var 'a (range 3)) ($var 'b (range 3))) null))
              ($var 'a (range 3)))
(check-equal? (first-unassigned-variable ($csp (list ($avar 'a (range 3)) ($var 'b (range 3))) null))
              ($var 'b (range 3)))
(check-false (first-unassigned-variable ($csp (list ($avar 'a (range 3)) ($avar 'b (range 3))) null)))

(check-equal?
 ;; no forward checking when no constraints
 ($csp-vars (forward-check ($csp (list ($avar 'a '(1)) ($var 'b (range 2))) null) 'a))
 (list ($avar 'a '(1)) ($var 'b '(0 1))))

(check-equal?
 ($csp-vars (forward-check (forward-check ($csp (list ($avar 'a '(1))  ($avar 'b '(0)) ($var 'c '(0 1 2)))
                                                (list ($constraint '(a c) (negate =))
                                                      ($constraint '(b c) (negate =)))) 'a) 'b))
 (list ($avar 'a '(1)) ($avar 'b '(0)) ($cvar 'c '(2) '(b a))))

(check-equal?
 ;; no inconsistency: b≠c not checked when fc is relative to a
 ($csp-vars (forward-check ($csp (list ($avar 'a '(1))  ($var 'b (range 2)) ($var 'c '(0)))
                                 (list ($constraint '(a b) (negate =))
                                       ($constraint '(b c) (negate =)))) 'a))
 (list ($avar 'a '(1)) ($cvar 'b '(0) '(a)) ($var 'c '(0))))

(check-equal?
 ;; no inconsistency: a≠b not checked when fc ignores a, which is already assigned
 ($csp-vars (forward-check ($csp (list ($avar 'a '(1))  ($avar 'b '(1)) ($var 'c (range 2)))
                                 (list ($constraint '(a b) (negate =))
                                       ($constraint '(b c) (negate =)))) 'b))
 (list ($avar 'a '(1)) ($avar 'b '(1)) ($cvar 'c '(0) '(b))))

(check-exn $backtrack?
           (λ () ($csp-vars (forward-check ($csp (list ($avar 'a '(1))
                                                       ($var 'b '(1)))
                                                 (list ($constraint '(a b) (negate =)))) 'a))))


(check-equal? ($csp-vars (forward-check ($csp (list ($var 'a '(0))
                                                    ($var 'b (range 3)))
                                              (list ($constraint '(a b) <))) 'a))
              (list ($var 'a '(0)) ($cvar 'b '(1 2) '(a))))

(check-equal?
 (parameterize ([current-inference forward-check])
   (length (solve* ($csp (list ($var 'x (range 3))
                               ($var 'y (range 3))
                               ($var 'z (range 3)))
                         (list ($constraint '(x y) <>)
                               ($constraint '(x z) <>)
                               ($constraint '(y z) <>)))))) 6)

(parameterize ([current-inference forward-check])
  (define vds (for/list ([k '(wa nt nsw q t v sa)])
                ($var k '(red green blue))))
  (define cs (list
              ($constraint '(wa nt) neq?)
              ($constraint '(wa sa) neq?)
              ($constraint '(nt sa) neq?)
              ($constraint '(nt q) neq?)
              ($constraint '(q sa) neq?)
              ($constraint '(q nsw) neq?)
              ($constraint '(nsw sa) neq?)
              ($constraint '(nsw v) neq?)
              ($constraint '(v sa) neq?)))
  (define csp ($csp vds cs))
  (check-equal? (length (solve* csp)) 18))


(define quarters (make-csp))
(add-vars! quarters '(dollars quarters) (range 26))
(add-constraint! quarters (λ (d q) (= 26 (+ d q))) '(dollars quarters))
(add-constraint! quarters (λ (d q) (= 17 (+ d (* 0.25 q)))) '(dollars quarters))
(check-equal? (time (solve quarters))
               '((dollars . 14) (quarters . 12)))


;; xsum
#|
# Reorganize the following numbers in a way that each line of
# 5 numbers sum to 27.
#
#       1   6
#        2 7
#         3
#        8 4
#       9   5
#
|#
(define xsum (make-csp))
(add-vars! xsum '(l1 l2 l3 l4 r1 r2 r3 r4 x) (range 1 10))
(add-pairwise-constraint! xsum < '(l1 l2 l3 l4))
(add-pairwise-constraint! xsum < '(r1 r2 r3 r4))
(add-constraint! xsum (λ (l1 l2 l3 l4 x) (= 27 (+ l1 l2 l3 l4 x))) '(l1 l2 l3 l4 x))
(add-constraint! xsum (λ (r1 r2 r3 r4 x)  (= 27 (+ r1 r2 r3 r4 x))) '(r1 r2 r3 r4 x))
(add-pairwise-constraint! xsum alldiff= '(l1 l2 l3 l4 r1 r2 r3 r4 x))

(check-equal? (length (time (solve* xsum))) 8)



;; send more money problem
#|
# Assign equal values to equal letters, and different values to
# different letters, in a way that satisfies the following sum:
#
#    SEND
#  + MORE
#  ------
#   MONEY
|#

(define (word-value . xs)
  (for/sum ([(x idx) (in-indexed (reverse xs))])
    (* x (expt 10 idx))))

(define smm (make-csp))
(add-vars! smm '(s e n d m o r y) (λ () (range 10)))
(add-constraint! smm positive? '(s))
(add-constraint! smm positive? '(m))
(add-constraint! smm (λ (d e y) (= (modulo (+ d e) 10) y)) '(d e y))
(add-constraint! smm (λ (n d r e y)
                       (= (modulo (+ (word-value n d) (word-value r e)) 100)
                          (word-value e y))) '(n d r e y))
(add-constraint! smm (λ (e n d o r y)
                       (= (modulo (+ (word-value e n d) (word-value o r e)) 1000) (word-value n e y))) '(e n d o r y))
(add-constraint! smm (λ (s e n d m o r y)
                       (= (+ (word-value s e n d) (word-value m o r e))
                          (word-value m o n e y))) '(s e n d m o r y))
(add-pairwise-constraint! smm alldiff= '(s e n d m o r y))
(check-equal? (time (solve smm)) '((s . 9) (e . 5) (n . 6) (d . 7) (m . 1) (o . 0) (r . 8) (y . 2)))


;; queens problem
;; place queens on chessboard so they do not intersect
(define queens (make-csp))
(define qs (for/list ([q 8]) (string->symbol (format "q~a" q))))
(define rows (range (length qs)))
(add-vars! queens qs rows)
(define (q-col q) (string->number (string-trim (symbol->string q) "q")))
(for* ([qs (in-combinations qs 2)])
  (match-define (list qa qb) qs)
  (match-define (list qa-col qb-col) (map q-col qs))
  (add-constraint! queens
                   (λ (qa-row qb-row)
                     (and 
                      (not (= (abs (- qa-row qb-row)) (abs (- qa-col qb-col)))) ; same diagonal?
                      (not (= qa-row qb-row)))) ; same row?
                   (list qa qb)))

(check-equal? 92 (length (time (solve* queens))))